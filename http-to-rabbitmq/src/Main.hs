{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.AMQP
import Configuration
import Control.Concurrent
import System.Posix.Signals
import Control.Exception
import Data.Pool
import Data.Word
import Data.Text.Encoding
import Control.Monad

maxChannels :: Word16
maxChannels = 10000

-- | Make a 'Wai.Application', given a 'Data.Pool Network.AMQP.Channel', that
-- simply forwards the body of each request on, over a 'Channel' from the 'Pool'.
mkApp :: Pool Channel -> Application
mkApp pool = \ req respond -> do
  msg <- strictRequestBody req
  let contentType = (liftM decodeUtf8) $
                    lookup "Content-Type" $
                    requestHeaders req
  _ <- safeRabbitAction pool $
    \chan ->
      publishMsg chan xoExchange xoKey
      newMsg { msgBody = msg
             , msgDeliveryMode = Just Persistent
             , msgContentType = contentType}
  respond $ responseLBS status200 [] ""

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
              closeChannel (fromIntegral maxChannels) 600 1

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
        run 3000 (mkApp chanPool))
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
       Left _ -> safeRabbitAction pool f
       Right a -> return a