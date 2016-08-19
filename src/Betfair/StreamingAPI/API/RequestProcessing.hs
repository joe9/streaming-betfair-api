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
import qualified Data.IntMap.Strict   as IntMap
import           Network.Connection
import           Safe
--
import           Betfair.StreamingAPI.API.AddId
import           Betfair.StreamingAPI.API.CommonTypes
import           Betfair.StreamingAPI.API.Context
import           Betfair.StreamingAPI.API.Log
import           Betfair.StreamingAPI.API.Request
import           Betfair.StreamingAPI.API.StreamingState
import           Betfair.StreamingAPI.API.ToRequest
import qualified Betfair.StreamingAPI.Requests.AuthenticationMessage     as A
import qualified Betfair.StreamingAPI.Requests.HeartbeatMessage          as H
import qualified Betfair.StreamingAPI.Requests.MarketSubscriptionMessage as M
import qualified Betfair.StreamingAPI.Requests.OrderSubscriptionMessage  as O
import qualified Betfair.StreamingAPI.Types.BettingType                  as BT
import qualified Betfair.StreamingAPI.Types.MarketFilter                 as MF

request :: (ToJSON b
           ,ToRequest b
           ,AddId b)
        => Context a -> b -> IO (Context a)
request c r =
  do let currentId = ssIdCounter (cState c)
         readyToSendRequest = addId r currentId
     b <- (groomedLog c To . L.toStrict . addCRLF . encode) readyToSendRequest
     connectionPut (cConnection c)
                   b
     return (c {cState =
                  (cState c) {ssIdCounter = succ currentId
                             ,ssRequests =
                                IntMap.insert currentId
                                              (toRequest readyToSendRequest)
                                              (ssRequests (cState c))}})

resendOldRequest
  :: (ToJSON b)
  => Context a -> b -> IO (Context a)
resendOldRequest c readyToSendRequest =
  do b <- (groomedLog c To . L.toStrict . addCRLF . encode) readyToSendRequest
     connectionPut (cConnection c)
                   b
     return c

heartbeat :: Context a -> IO (Context a)
heartbeat c = request c (def :: H.HeartbeatMessage)

authentication :: Context a -> IO (Context a)
authentication c =
  request c
          (def {A.session = ssSessionToken state
               ,A.appKey = ssAppKey state} :: A.AuthenticationMessage)
  where state = cState c

marketSubscription :: Context a
                   -> M.MarketSubscriptionMessage
                   -> IO (Context a)
marketSubscription c new =
  case (fmap snd .
        headMay .
        IntMap.toDescList .
        IntMap.filter isJust .
        IntMap.map (sameAsNewMarketSubscribeRequests new) . ssRequests . cState) c of
    Nothing    -> request c new
    (Just old) -> resendOldRequest c old

sameAsNewMarketSubscribeRequests
  :: M.MarketSubscriptionMessage
  -> Request
  -> Maybe M.MarketSubscriptionMessage
sameAsNewMarketSubscribeRequests new (MarketSubscribe old)
  | old {M.id = 0
        ,M.clk = Nothing
        ,M.initialClk = Nothing} ==
      new {M.id = 0
          ,M.clk = Nothing
          ,M.initialClk = Nothing} = Just old
  | otherwise = Nothing
sameAsNewMarketSubscribeRequests _ _ = Nothing

orderSubscription
  :: Context a -> O.OrderSubscriptionMessage -> IO (Context a)
orderSubscription c new =
  case (fmap snd .
        headMay .
        IntMap.toDescList .
        IntMap.filter isJust .
        IntMap.map (sameAsNewOrderSubscribeRequests new) . ssRequests . cState) c of
    Nothing    -> request c new
    (Just old) -> resendOldRequest c old

sameAsNewOrderSubscribeRequests
  :: O.OrderSubscriptionMessage -> Request -> Maybe O.OrderSubscriptionMessage
sameAsNewOrderSubscribeRequests new (OrderSubscribe old)
  | old {O.id = 0
        ,O.clk = Nothing
        ,O.initialClk = Nothing} ==
      new {O.id = 0
          ,O.clk = Nothing
          ,O.initialClk = Nothing} = Just old
  | otherwise = Nothing
sameAsNewOrderSubscribeRequests _ _ = Nothing

addCRLF :: L.ByteString -> L.ByteString
addCRLF a = a <> "\r" <> "\n"

marketIdsSubscription
  :: Context a -> [MarketId] -> IO (Context a)
marketIdsSubscription c [] = return c
marketIdsSubscription c mids =
  marketSubscription
    c
    ((def :: M.MarketSubscriptionMessage) {M.marketFilter =
                                             (def :: MF.MarketFilter) {MF.bettingTypes =
                                                                         [BT.ODDS]
                                                                      ,MF.marketIds =
                                                                         Just mids}})

bulkMarketsSubscription
  :: MF.MarketFilter -> Context a -> IO (Context a)
bulkMarketsSubscription mf c =
  marketSubscription c
                     ((def :: M.MarketSubscriptionMessage) {M.marketFilter = mf})
