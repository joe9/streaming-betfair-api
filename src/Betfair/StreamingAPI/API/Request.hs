{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Betfair.StreamingAPI.API.Request
  ( Request(..)
  ) where

import Protolude
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)

--
import qualified Betfair.StreamingAPI.Requests.AuthenticationMessage     as A
import qualified Betfair.StreamingAPI.Requests.HeartbeatMessage          as H
import qualified Betfair.StreamingAPI.Requests.MarketSubscriptionMessage as M
import qualified Betfair.StreamingAPI.Requests.OrderSubscriptionMessage  as O

data Request
  = Heartbeat H.HeartbeatMessage
  | Authentication A.AuthenticationMessage
  | MarketSubscribe M.MarketSubscriptionMessage
  | OrderSubscribe O.OrderSubscriptionMessage
  | UnknownRequest (Maybe Text)
                   (Maybe Text)
  deriving (Eq, Read, Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''Request)
