{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Requests.MarketSubscriptionMessage
  ( MarketSubscriptionMessage(..)
  , addMarketIdsToMarketSubscriptionMessage
  , defaultMarketSubscriptionMessage
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

import Betfair.StreamingAPI.API.AddId
import Betfair.StreamingAPI.Types.MarketDataFilter
import Betfair.StreamingAPI.Types.MarketFilter

data MarketSubscriptionMessage = MarketSubscriptionMessage
  { op                  :: Text
  , id                  :: Int -- Client generated unique id to link request with response (like json rpc)
  , segmentationEnabled :: Bool -- Segmentation Enabled - allow the server to send large sets of data in segments, instead of a single block
  , clk                 :: Maybe Text -- Token value delta (received in MarketChangeMessage) that should be passed to resume a subscription
  , heartbeatMs         :: Integer -- Heartbeat Milliseconds - the heartbeat rate (looped back on initial image after validation: bounds are 500 to 30000)
  , initialClk          :: Maybe Text -- Token value (received in initial MarketChangeMessage) that should be passed to resume a subscription
  , marketFilter        :: MarketFilter
  , conflateMs          :: Integer -- Conflate Milliseconds - the conflation rate (looped back on initial image after validation: bounds are 0 to 120000)
  , marketDataFilter    :: MarketDataFilter
  } deriving (Eq, Read, Show, Generic, Pretty)

$(deriveJSON
    defaultOptions {omitNothingFields = True}
    ''MarketSubscriptionMessage)

defaultMarketSubscriptionMessage :: MarketSubscriptionMessage
defaultMarketSubscriptionMessage =
  MarketSubscriptionMessage
    "marketSubscription"
    0
    True
    Nothing
    (30 * 1000)
    Nothing
    defaultMarketFilter
    0
    defaultMarketDataFilter

instance AddId MarketSubscriptionMessage where
  addId o i = o {id = i}

type MarketId = Text

addMarketIdsToMarketSubscriptionMessage :: [MarketId]
                                        -> MarketSubscriptionMessage
                                        -> MarketSubscriptionMessage
addMarketIdsToMarketSubscriptionMessage mids ms =
  ms
  { marketFilter =
      (marketFilter ms)
      {marketIds = Just mids, eventTypeIds = Nothing, marketTypes = Nothing}
  }
