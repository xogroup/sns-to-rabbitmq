{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.AMQP
import Configuration
import Control.Concurrent
import System.Posix.Signals
import Control.Exception

mkApp :: Connection -> Application
mkApp conn = \ req respond -> do
  chan <- openChannel conn
  respond $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, Web!"

main :: IO ()
main = do
  -- main is mostly cleanup/handler stuff
  connContainer <- newEmptyMVar
  originalHandlerContainer <- newEmptyMVar

  let handler = do
        conn <- takeMVar connContainer
        closeConnection conn
        original <- takeMVar originalHandlerContainer
        -- reinstall the original handler and immediately invoke it
        _ <- (installHandler sigTERM original Nothing)
        raiseSignal sigTERM

  originalHandler <- (installHandler sigTERM (Catch handler) Nothing)
  putMVar originalHandlerContainer originalHandler
  bracket
    (openConnection'' xoConnectionOpts)
    closeConnection
    (\conn -> do
        -- store the connection somewhere the signal handler can get to it
        putMVar connContainer conn

        -- we'll use a fresh channel for each request, but first we need
        -- to set up the queue and exchange
        chan <- openChannel conn
        _ <- declareQueue chan newQueue {queueName = xoQueue}
        declareExchange chan newExchange { exchangeName = xoExchange
                                         , exchangeType = "direct"}
        bindQueue chan xoQueue xoExchange xoKey

        run 3000 (mkApp conn))
