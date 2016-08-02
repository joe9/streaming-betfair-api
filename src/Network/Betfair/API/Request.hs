{-# OPTIONS_GHC -Wall   #-}

module Network.Betfair.API.Request
  (request, Request(..),heartbeat,authentication,marketSubscription,orderSubscription)
  where

import qualified Data.ByteString.Lazy as L

import Control.Monad.RWS
import Data.Aeson
import Data.Default
import Network.Connection

import WriterLog

import qualified Network.Betfair.Requests.AuthenticationMessage as A
import qualified Network.Betfair.Requests.HeartbeatMessage as H
import qualified Network.Betfair.Requests.MarketSubscriptionMessage  as M
import qualified Network.Betfair.Requests.OrderSubscriptionMessage       as O

data Request = Heartbeat H.HeartbeatMessage | Authentication A.AuthenticationMessage | MarkeSubscribe M.MarketSubscriptionMessage | OrderSubscribe O.OrderSubscriptionMessage

request :: ToJSON a
        => a -> RWST Connection Log s IO ()
request r =
  ask >>=
  (\c -> (groomedLog . L.toStrict . encode) r >>= lift . connectionPut c)

heartbeat :: Integer -> RWST Connection Log s IO ()
heartbeat i = request ( def {H.id = i})

authentication :: Integer -> String -> String -> RWST Connection Log s IO ()
  -- TODO fix this
-- authentication i s = request . def
authentication = undefined

marketSubscription
  :: M.MarketSubscriptionMessage -> RWST Connection Log s IO ()
marketSubscription = request

orderSubscription
  :: O.OrderSubscriptionMessage -> RWST Connection Log s IO ()
orderSubscription = request
