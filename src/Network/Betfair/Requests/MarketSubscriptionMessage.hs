{-# OPTIONS_GHC -Wall        #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Requests.MarketSubscriptionMessage
  (marketSubscription
  ,MarketSubscriptionMessage(..))
  where

import Control.Monad.RWS
import Data.Aeson.TH      (Options (omitNothingFields),
                           defaultOptions, deriveJSON)
import Network.Connection
-- import Data.Default.TH (deriveDefault)
import Data.Default
import Prelude      hiding (id)

import WriterLog

import Network.Betfair.API.Request
import Network.Betfair.Types.MarketDataFilter
import Network.Betfair.Types.MarketFilter

data MarketSubscriptionMessage =
  MarketSubscriptionMessage {op                  :: String
                            ,id                  :: Integer -- Client generated unique id to link request with response (like json rpc)
                            ,segmentationEnabled :: Bool -- Segmentation Enabled - allow the server to send large sets of data in segments, instead of a single block
                            ,clk                 :: String -- Token value delta (received in MarketChangeMessage) that should be passed to resume a subscription
                            ,heartbeatMs         :: Integer -- Heartbeat Milliseconds - the heartbeat rate (looped back on initial image after validation: bounds are 500 to 30000)
                            ,initialClk          :: String -- Token value (received in initial MarketChangeMessage) that should be passed to resume a subscription
                            ,marketFilter        :: MarketFilter
                            ,conflateMs          :: Integer -- Conflate Milliseconds - the conflation rate (looped back on initial image after validation: bounds are 0 to 120000)
                            ,marketDataFilter    :: MarketDataFilter}
  deriving (Eq,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''MarketSubscriptionMessage)

-- deriveDefault ''MarketSubscriptionMessage
instance Default MarketSubscriptionMessage where
  def =
    MarketSubscriptionMessage "MarketSubscription" 0 True "" 500 "" def 0 def

marketSubscription
  :: MarketSubscriptionMessage -> RWST Connection Log s IO ()
marketSubscription = request
