{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.ToRequest
  ( ToRequest
  , toRequest
  ) where

-- import Protolude
import           Betfair.StreamingAPI.API.Request
import qualified Betfair.StreamingAPI.Requests.AuthenticationMessage     as A
import qualified Betfair.StreamingAPI.Requests.HeartbeatMessage          as H
import qualified Betfair.StreamingAPI.Requests.MarketSubscriptionMessage as M
import qualified Betfair.StreamingAPI.Requests.OrderSubscriptionMessage  as O

class ToRequest a where
  toRequest :: a -> Request

instance ToRequest A.AuthenticationMessage where
  toRequest a = Authentication a

instance ToRequest H.HeartbeatMessage where
  toRequest a = Heartbeat a

instance ToRequest M.MarketSubscriptionMessage where
  toRequest a = MarketSubscribe a

instance ToRequest O.OrderSubscriptionMessage where
  toRequest a = OrderSubscribe a
