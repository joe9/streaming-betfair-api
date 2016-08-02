{-# OPTIONS_GHC -Wall     #-}

module Network.Betfair.API.RequestProcessing
  (request
  ,heartbeat
  ,authentication
  ,marketSubscription
  ,orderSubscription)
  where

import qualified Data.ByteString.Lazy as L

import Control.Monad.RWS
import Data.Aeson
import Data.Default
import Network.Connection

import WriterLog

import qualified Network.Betfair.Requests.AuthenticationMessage     as A
import qualified Network.Betfair.Requests.HeartbeatMessage          as H
import qualified Network.Betfair.Requests.MarketSubscriptionMessage as M
import qualified Network.Betfair.Requests.OrderSubscriptionMessage  as O

import Network.Betfair.API.AddId
import Network.Betfair.API.Config
import Network.Betfair.API.StreamingState

request
  :: (ToJSON a
     ,AddId a)
  => a -> RWST Connection Log StreamingState IO ()
request r =
  do s <- get
     let currentId = ssIdCounter s
     put (s {ssIdCounter = succ currentId})
     b <- (groomedLog . L.toStrict . encode) (addId r currentId)
     connection <- ask
     lift (connectionPut connection b)

heartbeat
  :: RWST Connection Log StreamingState IO ()
heartbeat = request (def :: H.HeartbeatMessage)

authentication
  :: RWST Connection Log StreamingState IO ()
authentication =
  do s <- get
     request (def {A.session = ssSessionToken s
                  ,A.appKey = (delayedAppKey . ssConfig) s} :: A.AuthenticationMessage)

marketSubscription
  :: M.MarketSubscriptionMessage -> RWST Connection Log StreamingState IO ()
marketSubscription = request

orderSubscription
  :: O.OrderSubscriptionMessage -> RWST Connection Log StreamingState IO ()
orderSubscription = request
