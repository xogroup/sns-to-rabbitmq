{-# LANGUAGE OverloadedStrings #-}
module Configuration
  (
    xoConnectionOpts
  , xoQueue
  , xoExchange
  , xoKey
  ) where

import Network.AMQP(ConnectionOpts(..), plain, defaultConnectionOpts)
import Data.Text(Text)

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
xoExchange = "logstashExchange"

xoKey :: Text
xoKey = "logstash"
