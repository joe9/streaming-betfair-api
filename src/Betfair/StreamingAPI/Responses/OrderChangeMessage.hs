{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Betfair.StreamingAPI.Responses.OrderChangeMessage
  ( OrderChangeMessage(..)
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

import Betfair.StreamingAPI.Types.ChangeType
import Betfair.StreamingAPI.Types.OrderMarketChange
import Betfair.StreamingAPI.Types.SegmentType

data OrderChangeMessage = OrderChangeMessage
  { op          :: Text
  , id          :: Integer -- Client generated unique id to link request with response (like json rpc)
  , ct          :: ChangeType -- Change Type - set to indicate the type of change - if null this is a delta),
  , clk         :: Text -- Token value (non-null) should be stored and passed in a MarketSubscriptionMessage to resume subscription (in case of disconnect)
  , status      :: Maybe Int --  By default, when the stream data is up to date the value is set to null and will be set to 503 when the stream data is unreliable (i.e. not all bets and markets changes will be reflected on the stream) due to an increase in push latency.  Clients shouldn't disconnect if status 503 is returned; when the stream recovers updates will be sent containing the latest data.  The status is sent per each subscription on heartbeats and change messages.
  , heartbeatMs :: Integer -- Heartbeat Milliseconds - the heartbeat rate (may differ from requested: bounds are 500 to 30000),
  , pt          :: Integer -- Publish Time (in millis since epoch) that the changes were generated,
  , oc          :: [OrderMarketChange] -- OrderMarketChanges - the modifications to account's orders (will be null on a heartbeat
  , initialClk  :: Text -- Token value (non-null) should be stored and passed in a MarketSubscriptionMessage to resume subscription (in case of disconnect)
  , conflateMs  :: Integer -- Conflate Milliseconds - the conflation rate (may differ from that requested if subscription is delayed),
  , segmentType :: SegmentType -- Segment Type - if the change is split into multiple segments, this denotes the beginning and end of a change, and segments in between. Will be null if data is not segmented,
  } deriving (Eq, Read, Show, Generic, Pretty)

$(deriveJSON defaultOptions {omitNothingFields = True} ''OrderChangeMessage)
