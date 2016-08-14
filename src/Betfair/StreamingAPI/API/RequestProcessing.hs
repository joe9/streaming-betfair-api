{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.RequestProcessing
  (request
  ,heartbeat
  ,authentication
  ,marketSubscription
  ,marketIdsSubscription
  ,bulkMarketsSubscription
  ,orderSubscription
  ,addCRLF)
  where

import           BasicPrelude
import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Default
import           Network.Connection
--
import           Betfair.StreamingAPI.API.AddId
import           Betfair.StreamingAPI.API.CommonTypes
import           Betfair.StreamingAPI.API.Context
import           Betfair.StreamingAPI.API.Log
import           Betfair.StreamingAPI.API.StreamingState
import qualified Betfair.StreamingAPI.Requests.AuthenticationMessage     as A
import qualified Betfair.StreamingAPI.Requests.HeartbeatMessage          as H
import qualified Betfair.StreamingAPI.Requests.MarketSubscriptionMessage as M
import qualified Betfair.StreamingAPI.Requests.OrderSubscriptionMessage  as O
import qualified Betfair.StreamingAPI.Types.BettingType                  as BT
import qualified Betfair.StreamingAPI.Types.MarketFilter                 as MF

request :: (ToJSON b
           ,AddId b)
        => (Context a) -> b -> IO (Context a)
request c r =
  do let currentId = ssIdCounter (cState c)
     b <- (groomedLog c To . L.toStrict . addCRLF . encode) (addId r currentId)
     connectionPut (cConnection c)
                   b
     return (c {cState = (cState c) {ssIdCounter = succ currentId}})

heartbeat :: (Context a) -> IO (Context a)
heartbeat c = request c (def :: H.HeartbeatMessage)

authentication :: (Context a) -> IO (Context a)
authentication c =
  request c
          (def {A.session = ssSessionToken state
               ,A.appKey = ssAppKey state} :: A.AuthenticationMessage)
  where state = cState c

marketSubscription :: (Context a)
                   -> M.MarketSubscriptionMessage
                   -> IO (Context a)
marketSubscription c = request c

orderSubscription :: (Context a)
                  -> O.OrderSubscriptionMessage
                  -> IO (Context a)
orderSubscription c = request c

addCRLF :: L.ByteString -> L.ByteString
addCRLF a = a <> "\r" <> "\n"

marketIdsSubscription
  :: (Context a) -> [MarketId] -> IO (Context a)
marketIdsSubscription c [] = return c
marketIdsSubscription c mids =
  marketSubscription
    c
    ((def :: M.MarketSubscriptionMessage) {M.marketFilter =
                                             ((def :: MF.MarketFilter) {MF.bettingTypes =
                                                                          [BT.ODDS]
                                                                       ,MF.marketIds =
                                                                          Just mids})})

-- event types
-- 1 Soccer 13904
-- 2 Tennis 3615
-- 3 Golf 89
-- 4 Cricket 170
-- 5 Rugby Union 10
-- 6 Boxing 51
-- 7 Horse Racing 599
-- 8 Motor Sport 11
-- 10 Special Bets 16
-- 11 Cycling 17
-- 1477 Rugby League 28
-- 3503 Darts 2
-- 3988 Athletics 126
-- 4339 Greyhound Racing 181
-- 6231 Financial Bets 3
-- 6422 Snooker 3
-- 6423 American Football 32
-- 7511 Baseball 68
-- 7522 Basketball 143
-- 7523 Hockey 12
-- 7524 Ice Hockey 1
-- 136332 Chess 15
-- 315220 Poker 1
-- 468328 Handball 48
-- 620576 Swimming 4
-- 627555 Badminton 10
-- 998917 Volleyball 26
-- 2152880 Gaelic Games 24
-- 2378961 Politics 43
-- 2593174 Table Tennis 4
-- 2872194 Beach Volleyball 6
-- 2901849 Water Polo 16
-- 26420387 Mixed Martial Arts 77
-- 27589895 Olympics 2016 156
bulkMarketsSubscription
  :: (Context a) -> IO (Context a)
bulkMarketsSubscription c =
  marketSubscription
    c
    ((def :: M.MarketSubscriptionMessage)
     {M.marketFilter = ((def :: MF.MarketFilter) {MF.bettingTypes = [BT.ODDS]
                                                ,MF.turnInPlayEnabled = Just True
                                                ,MF.marketTypes = Just ["MATCH_ODDS"]
                                                ,MF.eventTypeIds =
                                                  (Just . fmap show)
                                                    ([2 :: Integer
                                                        ,4
                                                        ,5
                                                        ,6423
                                                        ,7511
                                                        ,7522])})})
