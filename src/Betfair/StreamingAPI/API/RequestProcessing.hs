{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.RequestProcessing
  (request
  ,heartbeat
  ,authentication
  ,marketSubscription
  ,marketIdsSubscription
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
import           Betfair.StreamingAPI.API.Context
import           Betfair.StreamingAPI.API.Log
import           Betfair.StreamingAPI.API.StreamingState
import qualified Betfair.StreamingAPI.Requests.AuthenticationMessage     as A
import qualified Betfair.StreamingAPI.Requests.HeartbeatMessage          as H
import qualified Betfair.StreamingAPI.Requests.MarketSubscriptionMessage as M
import qualified Betfair.StreamingAPI.Requests.OrderSubscriptionMessage  as O
import qualified Betfair.StreamingAPI.Types.BettingType                  as BT
import qualified Betfair.StreamingAPI.Types.MarketFilter                 as MF

request :: (ToJSON a
           ,AddId a)
        => Context -> a -> IO Context
request c r =
  do let currentId = ssIdCounter (cState c)
     b <- (groomedLog c To . L.toStrict . addCRLF . encode) (addId r currentId)
     connectionPut (cConnection c)
                   b
     return (c {cState = (cState c) {ssIdCounter = succ currentId}})

heartbeat :: Context -> IO Context
heartbeat c = request c (def :: H.HeartbeatMessage)

authentication :: Context -> IO Context
authentication c =
  request c
          (def {A.session = ssSessionToken state
               ,A.appKey = ssAppKey state} :: A.AuthenticationMessage)
  where state = cState c

marketSubscription
  :: Context -> M.MarketSubscriptionMessage -> IO Context
marketSubscription c = request c

orderSubscription
  :: Context -> O.OrderSubscriptionMessage -> IO Context
orderSubscription c = request c

addCRLF :: L.ByteString -> L.ByteString
addCRLF a = a <> "\r" <> "\n"

marketIdsSubscription
  :: Context -> [MarketId] -> IO Context
marketIdsSubscription c [] = return c
marketIdsSubscription c mids =
  marketSubscription
    c
    ((def :: M.MarketSubscriptionMessage) {M.marketFilter =
                                             ((def :: MF.MarketFilter) {MF.bettingTypes =
                                                                          [BT.ODDS]
                                                                       ,MF.marketIds =
                                                                          Just mids})})
