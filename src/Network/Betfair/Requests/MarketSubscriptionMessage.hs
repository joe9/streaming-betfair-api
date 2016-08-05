{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Requests.MarketSubscriptionMessage
  (MarketSubscriptionMessage(..))
  where

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
-- import Data.Default.TH (deriveDefault)
import Data.Default
import Network.Betfair.API.AddId
import Network.Betfair.Types.MarketDataFilter
import Network.Betfair.Types.MarketFilter
import Prelude                                hiding (id)

data MarketSubscriptionMessage =
  MarketSubscriptionMessage {op                  :: Text
                            ,id                  :: Integer -- Client generated unique id to link request with response (like json rpc)
                            ,segmentationEnabled :: Bool -- Segmentation Enabled - allow the server to send large sets of data in segments, instead of a single block
                            ,clk                 :: Maybe Text -- Token value delta (received in MarketChangeMessage) that should be passed to resume a subscription
                            ,heartbeatMs         :: Integer -- Heartbeat Milliseconds - the heartbeat rate (looped back on initial image after validation: bounds are 500 to 30000)
                            ,initialClk          :: Maybe Text -- Token value (received in initial MarketChangeMessage) that should be passed to resume a subscription
                            ,marketFilter        :: MarketFilter
                            ,conflateMs          :: Integer -- Conflate Milliseconds - the conflation rate (looped back on initial image after validation: bounds are 0 to 120000)
                            ,marketDataFilter    :: MarketDataFilter}
  deriving (Eq,Read,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''MarketSubscriptionMessage)

-- deriveDefault ''MarketSubscriptionMessage
instance Default MarketSubscriptionMessage where
  def =
    MarketSubscriptionMessage "marketSubscription" 0 True Nothing 500 Nothing def 0 def

instance AddId MarketSubscriptionMessage where
  addId o i = o {id = i}
