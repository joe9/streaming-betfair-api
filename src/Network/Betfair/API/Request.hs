{-# LANGUAGE OverloadedStrings #-}

module Network.Betfair.API.Request
  (Request(..))
  where

import qualified Network.Betfair.Requests.AuthenticationMessage     as A
import qualified Network.Betfair.Requests.HeartbeatMessage          as H
import qualified Network.Betfair.Requests.MarketSubscriptionMessage as M
import qualified Network.Betfair.Requests.OrderSubscriptionMessage  as O

data Request
  = Heartbeat H.HeartbeatMessage
  | Authentication A.AuthenticationMessage
  | MarkeSubscribe M.MarketSubscriptionMessage
  | OrderSubscribe O.OrderSubscriptionMessage
  deriving (Eq,Read,Show)
