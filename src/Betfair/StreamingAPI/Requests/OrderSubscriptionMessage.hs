{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Requests.OrderSubscriptionMessage
  (OrderSubscriptionMessage(..))
  where
import BasicPrelude

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Data.Text
-- import Data.Default.TH (deriveDefault)
import Data.Default
import Betfair.StreamingAPI.API.AddId
import Betfair.StreamingAPI.Types.OrderFilter (OrderFilter)
import Prelude                           hiding (id)

data OrderSubscriptionMessage =
  OrderSubscriptionMessage {op                  :: Text
                           ,id                  :: Integer -- Client generated unique id to link request with response (like json rpc)
                           ,segmentationEnabled :: Bool -- Segmentation Enabled - allow the server to send large sets of data in segments, instead of a single block
                           ,orderFilter         :: OrderFilter
                           ,clk                 :: Text -- Token value delta (received in MarketChangeMessage) that should be passed to resume a subscription
                           ,heartbeatMs         :: Integer -- Heartbeat Milliseconds - the heartbeat rate (looped back on initial image after validation: bounds are 500 to 30000)
                           ,initialClk          :: Text -- Token value (received in initial MarketChangeMessage) that should be passed to resume a subscription
                           ,conflateMs          :: Integer -- Conflate Milliseconds - the conflation rate (looped back on initial image after validation: bounds are 0 to 120000)
                           }
  deriving (Eq,Read,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''OrderSubscriptionMessage)

-- deriveDefault ''OrderSubscriptionMessage
instance Default OrderSubscriptionMessage where
  def = OrderSubscriptionMessage "OrderSubscription" 0 True def "" 500 "" 0

instance AddId OrderSubscriptionMessage where
  addId o i = o {id = i}
