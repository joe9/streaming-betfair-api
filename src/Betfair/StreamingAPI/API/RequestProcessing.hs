{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.RequestProcessing
  (request
  ,heartbeat
  ,authentication
  ,marketSubscription
  ,marketIdsSubscription
  ,bulkMarketsSubscription
  ,resubscribe
  ,lastMarketSubscriptionMessage
  ,orderSubscription
  ,addCRLF)
  where

import BasicPrelude
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.Default
import qualified Data.IntMap.Strict as IntMap
import Network.Connection
import Safe
--
import Betfair.StreamingAPI.API.AddId
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Context
import Betfair.StreamingAPI.API.Log
import Betfair.StreamingAPI.API.Request
import Betfair.StreamingAPI.API.StreamingState
import Betfair.StreamingAPI.API.ToRequest
import qualified
       Betfair.StreamingAPI.Requests.AuthenticationMessage as A
import qualified Betfair.StreamingAPI.Requests.HeartbeatMessage
       as H
import qualified
       Betfair.StreamingAPI.Requests.MarketSubscriptionMessage as M
import qualified
       Betfair.StreamingAPI.Requests.OrderSubscriptionMessage as O
import qualified Betfair.StreamingAPI.Types.BettingType as BT
import qualified Betfair.StreamingAPI.Types.MarketFilter as MF

request
    :: (ToJSON b, ToRequest b, AddId b)
    => Context -> b -> IO (Context)
request c r = do
    let currentId = ssIdCounter (cState c)
        readyToSendRequest = addId r currentId
    b <- (groomedLog c To . L.toStrict . addCRLF . encode) readyToSendRequest
    connectionPut (cConnection c) b
    return (c {cState =
                 (cState c) {ssIdCounter = succ currentId
                            ,ssRequests =
                               IntMap.insert currentId
                                             (toRequest readyToSendRequest)
                                             (ssRequests (cState c))}})

resendOldRequest
    :: (ToJSON b)
    => Context -> b -> IO (Context)
resendOldRequest c readyToSendRequest = do
    b <- (groomedLog c To . L.toStrict . addCRLF . encode) readyToSendRequest
    connectionPut (cConnection c) b
    return c

heartbeat :: Context -> IO (Context)
heartbeat c = request c (def :: H.HeartbeatMessage)

authentication :: Context -> IO (Context)
authentication c =
    request
        c
        (def
         { A.session = ssSessionToken state
         , A.appKey = ssAppKey state
         } :: A.AuthenticationMessage)
  where
    state = cState c

marketSubscription :: Context -> M.MarketSubscriptionMessage -> IO (Context)
marketSubscription c new = do
  timeInMicros <- timeInMicroseconds
  let cn = c {cState = (cState c) {ssLastMarketSubscriptionMessageSentAt = timeInMicros}}
  case (lastMay .
          IntMap.elems .
          IntMap.filter isJust .
          IntMap.map (sameAsNewMarketSubscribeRequests new) .
          ssRequests . cState)
             c of
        Nothing -> request cn new
        (Just old) -> resendOldRequest cn old

sameAsNewMarketSubscribeRequests :: M.MarketSubscriptionMessage
                                 -> Request
                                 -> Maybe M.MarketSubscriptionMessage
sameAsNewMarketSubscribeRequests new (MarketSubscribe old)
  | old
   { M.id = 0
   , M.clk = Nothing
   , M.initialClk = Nothing
   } ==
        new
        { M.id = 0
        , M.clk = Nothing
        , M.initialClk = Nothing
        } =
      Just old
  | otherwise = Nothing
sameAsNewMarketSubscribeRequests _ _ = Nothing

orderSubscription :: Context -> O.OrderSubscriptionMessage -> IO (Context)
orderSubscription c new =
    case (lastMay .
          IntMap.elems .
          IntMap.filter isJust .
          IntMap.map (sameAsNewOrderSubscribeRequests new) .
          ssRequests . cState)
             c of
        Nothing -> request c new
        (Just old) -> resendOldRequest c old

sameAsNewOrderSubscribeRequests :: O.OrderSubscriptionMessage
                                -> Request
                                -> Maybe O.OrderSubscriptionMessage
sameAsNewOrderSubscribeRequests new (OrderSubscribe old)
  | old
   { O.id = 0
   , O.clk = Nothing
   , O.initialClk = Nothing
   } ==
        new
        { O.id = 0
        , O.clk = Nothing
        , O.initialClk = Nothing
        } =
      Just old
  | otherwise = Nothing
sameAsNewOrderSubscribeRequests _ _ = Nothing

addCRLF :: L.ByteString -> L.ByteString
addCRLF a = a <> "\r" <> "\n"

marketIdsSubscription :: Context -> [MarketId] -> IO (Context)
marketIdsSubscription c [] = return c
marketIdsSubscription c mids =
    marketSubscription
        c
        ((def :: M.MarketSubscriptionMessage)
         { M.marketFilter = (def :: MF.MarketFilter)
           { MF.bettingTypes = [BT.ODDS]
           , MF.marketIds = Just mids
           }
         })

bulkMarketsSubscription :: MF.MarketFilter -> Context -> IO (Context)
bulkMarketsSubscription mf c =
    marketSubscription
        c
        ((def :: M.MarketSubscriptionMessage)
         { M.marketFilter = mf
         })

resubscribe :: M.MarketSubscriptionMessage -> Context -> IO (Context)
resubscribe new c = marketSubscription c new

lastMarketSubscriptionMessage :: Context -> Maybe M.MarketSubscriptionMessage
lastMarketSubscriptionMessage c
  | IntMap.null marketSubscriptionMessages = Nothing
  | otherwise = (snd . IntMap.findMax) marketSubscriptionMessages
  where
    marketSubscriptionMessages =
        (IntMap.filter isJust .
         IntMap.map marketSubscriptionMessage . ssRequests . cState)
            c

marketSubscriptionMessage :: Request -> Maybe M.MarketSubscriptionMessage
marketSubscriptionMessage (MarketSubscribe m) = Just m
marketSubscriptionMessage _ = Nothing
