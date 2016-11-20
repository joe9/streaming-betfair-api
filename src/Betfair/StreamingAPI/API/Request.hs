{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Betfair.StreamingAPI.API.Request
  ( Request(..)
  ) where

import Text.PrettyPrint.GenericPretty
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Protolude

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
  deriving (Eq, Read, Show, Generic, Pretty)

$(deriveJSON defaultOptions {omitNothingFields = True} ''Request)
