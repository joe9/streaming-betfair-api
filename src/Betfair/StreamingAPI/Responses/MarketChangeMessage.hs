{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Betfair.StreamingAPI.Responses.MarketChangeMessage
  (MarketChangeMessage(..))
  where
import BasicPrelude

import Data.Aeson.TH                      (Options (omitNothingFields),
                                           defaultOptions, deriveJSON)
import Data.Text
import Betfair.StreamingAPI.Types.ChangeType
import Betfair.StreamingAPI.Types.MarketChange
import Betfair.StreamingAPI.Types.SegmentType

data MarketChangeMessage =
  MarketChangeMessage {op          :: Text
                      ,id          :: Integer -- Client generated unique id to link request with response (like json rpc)
                      ,ct          :: Maybe ChangeType -- Change Type - set to indicate the type of change - if null this is a delta),
                      ,clk         :: Maybe Text -- Token value (non-null) should be stored and passed in a MarketSubscriptionMessage to resume subscription (in case of disconnect)
                      ,heartbeatMs :: Maybe Integer -- Heartbeat Milliseconds - the heartbeat rate (may differ from requested: bounds are 500 to 30000),
                      ,pt          :: Integer -- Publish Time (in millis since epoch) that the changes were generated,
                      ,initialClk  :: Maybe Text -- Token value (non-null) should be stored and passed in a MarketSubscriptionMessage to resume subscription (in case of disconnect)
                      ,mc          :: Maybe [MarketChange] -- MarketChanges - the modifications to markets (will be null on a heartbeat,
                      ,conflateMs  :: Maybe Integer -- Conflate Milliseconds - the conflation rate (may differ from that requested if subscription is delayed),
                      ,segmentType :: Maybe SegmentType -- Segment Type - if the change is split into multiple segments, this denotes the beginning and end of a change, and segments in between. Will be null if data is not segmented,
                      }
  deriving (Eq,Read,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''MarketChangeMessage)