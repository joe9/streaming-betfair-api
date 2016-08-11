{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Request
  (Request(..))
  where

import           BasicPrelude
import qualified Betfair.StreamingAPI.Requests.AuthenticationMessage     as A
import qualified Betfair.StreamingAPI.Requests.HeartbeatMessage          as H
import qualified Betfair.StreamingAPI.Requests.MarketSubscriptionMessage as M
import qualified Betfair.StreamingAPI.Requests.OrderSubscriptionMessage  as O

data Request
  = Heartbeat H.HeartbeatMessage
  | Authentication A.AuthenticationMessage
  | MarkeSubscribe M.MarketSubscriptionMessage
  | OrderSubscribe O.OrderSubscriptionMessage
  deriving (Eq,Read,Show)
