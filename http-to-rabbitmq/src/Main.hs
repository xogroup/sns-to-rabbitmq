{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Client hiding (Request, Response)
import Network.HTTP.Client.TLS
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.HttpAuth
import Network.AMQP
import Configuration
import Control.Concurrent
import System.Posix.Signals
import Control.Exception
import Data.Pool
import Data.Word
import Data.Text.Encoding
import Data.Aeson
import Control.Monad
import qualified Data.Map.Lazy as Map


maxChannels :: Word16
maxChannels = 10000

-- | Make a 'Wai.Application', given a 'Data.Pool Network.AMQP.Channel', that
-- simply forwards the body of each request on, over a 'Channel' from the 'Pool'.
mkApp :: Pool Channel -> Application
mkApp pool = \ req respond -> do
  if isHealthCheck req then respondBlank200 respond else do
    msg <- strictRequestBody req
    let contentType = (liftM decodeUtf8) $
                      lookup "Content-Type" $
                      Network.Wai.requestHeaders req
    let amzMsgType = (liftM decodeUtf8) $
                     lookup "x-amz-sns-message-type" $
                     Network.Wai.requestHeaders req
    case amzMsgType of
      Just "SubscriptionConfirmation" -> do
        manager <- newManager tlsManagerSettings
        request <- parseRequest amzSubscriptionURL
        _ <- httpLbs request manager
        respondBlank200 respond
          where
            amzSubscriptionURL =
              case Map.lookup "SubscribeURL" amzJsonMap of
                Just u -> u
            amzJsonMap =
              case decode msg :: Maybe (Map.Map String String) of
                Just m -> m
      Just "Notification" -> do
        _ <- safeRabbitAction pool $
          \chan ->
            publishMsg chan xoExchange xoKey
            newMsg { msgBody = msg
                   , msgDeliveryMode = Just Persistent
                   , msgContentType = contentType}
        respondBlank200 respond
      _ -> respond $ responseLBS status400 [] ""

respondBlank200 :: (Response -> IO ResponseReceived) -> IO ResponseReceived
respondBlank200 respond = respond $ responseLBS status200 [] ""

authentication :: Middleware
authentication = basicAuth
                 (\u p -> return $ u == xoWaiUsername && p == xoWaiPassword)
                 ("realm" {authIsProtected = return . not . isHealthCheck}
                   :: AuthSettings)

isHealthCheck :: Request -> Bool
isHealthCheck req = case (requestMethod req, pathInfo req) of
                      (m, ["healthy"]) | m == methodGet -> True
                      _ -> False

main :: IO ()
main = do
  -- main is mostly cleanup/handler stuff
  originalHandlerContainer <- newEmptyMVar

  -- connPool: one "stripe" of ten connections.
  connPool <- createPool (openConnection'' xoConnectionOpts)
              closeConnection 1 5 10

  -- chanPool: many "stripes" but only one channel per stripe, because
  -- each thread should only need one channel.
  chanPool <- createPool (safeRabbitAction connPool getConfirmModeChan)
              closeChannel (fromIntegral maxChannels) 3 1

  let handler = do
        destroyAllResources connPool
        original <- takeMVar originalHandlerContainer
        -- reinstall the original handler and immediately invoke it
        _ <- (installHandler sigTERM original Nothing)
        raiseSignal sigTERM

  originalHandler <- (installHandler sigTERM (Catch handler) Nothing)
  putMVar originalHandlerContainer originalHandler

  finally
    (do
        safeRabbitAction chanPool initializeQueue
        run 3000 $ authentication $ mkApp chanPool)
    (destroyAllResources connPool)

-- | Get a channel and put it in publisher confirm mode. This function may
-- fail and/or throw an exception.
getConfirmModeChan :: Connection -> IO Channel
getConfirmModeChan conn = do
  chan <- openChannel conn
  confirmSelect chan False
  return chan

-- | Initialize the configured queue and exchange. This function may fail
-- and/or throw an exception.
initializeQueue :: Channel -> IO ()
initializeQueue chan = do
  _ <- declareQueue chan newQueue {queueName = xoQueue}
  declareExchange chan newExchange { exchangeName = xoExchange
                                   , exchangeType = "direct"}
  bindQueue chan xoQueue xoExchange xoKey

-- the exception predicate to pass to tryJust in safeRabbitAction.
selectClosedExceptions :: AMQPException -> Maybe AMQPException
selectClosedExceptions ex = case ex of
  e@(ChannelClosedException _) -> Just e
  e@(ConnectionClosedException _) -> Just e
  _ -> Nothing

-- | Perform an action repeatedly until no ChannelClosedException
-- or ConnectionClosedException occurs.
safeRabbitAction :: Pool b -> (b -> IO a) -> IO a
safeRabbitAction pool f =
  do r <- tryJust selectClosedExceptions $
          withResource pool f
     -- by Haskell convention, 'Left e' means an error 'e' occurred; 'Right a'
     -- means no error occurred and 'a' is the answer. Using 'withResource'
     -- above ensures that the 'Connection' or 'Channel' involved is destroyed
     -- if any exception whatsoever occurs.
     case r of
       Left _ -> threadDelay 500000 >> safeRabbitAction pool f
       Right a -> return a
