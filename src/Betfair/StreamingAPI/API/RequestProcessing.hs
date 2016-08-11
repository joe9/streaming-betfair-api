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
import BasicPrelude

import           Control.Monad.RWS
import           Data.Aeson
import qualified Data.ByteString.Lazy                               as L
import           Data.Default
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
import           Network.Connection

request
  :: (ToJSON a
     ,AddId a)
  => a -> RWST Context () StreamingState IO ()
request r =
  do s <- get
     let currentId = ssIdCounter s
     put (s {ssIdCounter = succ currentId})
     b <- (groomedLog To . L.toStrict . addCRLF . encode) (addId r currentId)
     connection <- fmap cConnection ask
     lift (connectionPut connection b)

heartbeat :: RWST Context () StreamingState IO ()
heartbeat = request (def :: H.HeartbeatMessage)

authentication
  :: RWST Context () StreamingState IO ()
authentication =
  do s <- get
     request (def {A.session = ssSessionToken s
                  ,A.appKey = ssAppKey s} :: A.AuthenticationMessage)

marketSubscription
  :: M.MarketSubscriptionMessage -> RWST Context () StreamingState IO ()
marketSubscription = request

orderSubscription
  :: O.OrderSubscriptionMessage -> RWST Context () StreamingState IO ()
orderSubscription = request

addCRLF :: L.ByteString -> L.ByteString
addCRLF a = a <> "\r" <> "\n"

marketIdsSubscription
  :: [MarketId] -> RWST Context () StreamingState IO ()
marketIdsSubscription [] = return ()
marketIdsSubscription mids =
  marketSubscription
    ((def :: M.MarketSubscriptionMessage) {M.marketFilter =
                                             ((def :: MF.MarketFilter) {MF.bettingTypes =
                                                                          [BT.ODDS]
                                                                       ,MF.marketIds =
                                                                          Just mids})})
