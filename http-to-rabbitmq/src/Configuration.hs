{-# LANGUAGE OverloadedStrings #-}
module Configuration
  (
    xoConnectionOpts
  , xoQueue
  , xoExchange
  , xoKey
  , xoWaiUsername
  , xoWaiPassword
  ) where

import Network.AMQP(ConnectionOpts(..), plain, defaultConnectionOpts)
import Data.Text
import qualified Data.ByteString as B

xoConnectionOpts :: ConnectionOpts
xoConnectionOpts = defaultConnectionOpts {
  coServers   = [("***REMOVED***.rmq.cloudamqp.com", 5672)]
  , coVHost   = "***REMOVED***"
  -- I don't like having this in a config file. Note this password is
  -- easily rotatable at CloudAMQP Control Panel.
  , coAuth    = [plain "***REMOVED***" "***REMOVED***"]
  }

xoQueue :: Text
xoQueue = "logstashQueue"

xoExchange :: Text
xoExchange = "logstashFanout"

xoKey :: Text
xoKey = "logstash"

xoWaiUsername :: B.ByteString
xoWaiUsername = "***REMOVED***"

xoWaiPassword :: B.ByteString
xoWaiPassword = "***REMOVED***"
