{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Betfair.BulkStreamingAPI
-- from this file
  ( sampleStart
  , startStreaming
  , initializeContext
  , Response(..)
  , ResponseException(..)
  , Context(..)
  , StreamingState(..)
   -- Common Types
  , MarketId
  , MarketName
  , EventName
  , SessionToken
  , AppKey
   -- Other Types explicitly defined in modules
  , PersistenceType
  , Order
  , OrderMarketChange
  , ChangeType(..)
  , MarketFilter
  , MarketChange
  , OrderRunnerChange
  , Field
  , MarketDefinition
  , OrderFilter
  , Side
  , RequestStatus
  , RunnerChange
  , OrderType
  , ErrorCode
  , BettingType
  , RunnerDefinition
  , OrderStatus
  , SegmentType
  , MarketStatus
  , MarketDataFilter
  , RunnerStatus
  ) where

import BasicPrelude            hiding (bracket, finally)
import Control.Exception.Safe
import Data.Default
import Data.String.Conversions
import Network.Connection
import Network.Socket

--
import           Betfair.StreamingAPI.API.AddId
import           Betfair.StreamingAPI.API.CommonTypes
import           Betfair.StreamingAPI.API.Config
import           Betfair.StreamingAPI.API.Context
import           Betfair.StreamingAPI.API.Log
import           Betfair.StreamingAPI.API.Request
import           Betfair.StreamingAPI.API.RequestProcessing
import           Betfair.StreamingAPI.API.Response
import           Betfair.StreamingAPI.API.ResponseException
import           Betfair.StreamingAPI.API.ResponseProcessing
import           Betfair.StreamingAPI.API.StreamingState
import           Betfair.StreamingAPI.Requests.AuthenticationMessage
import           Betfair.StreamingAPI.Requests.HeartbeatMessage
import           Betfair.StreamingAPI.Requests.MarketSubscriptionMessage
import           Betfair.StreamingAPI.Requests.OrderSubscriptionMessage
import           Betfair.StreamingAPI.Responses.ConnectionMessage
import           Betfair.StreamingAPI.Responses.MarketChangeMessage
import           Betfair.StreamingAPI.Responses.OrderChangeMessage
import           Betfair.StreamingAPI.Responses.StatusMessage
import           Betfair.StreamingAPI.Types.BettingType
import           Betfair.StreamingAPI.Types.ChangeType
import           Betfair.StreamingAPI.Types.ErrorCode
import           Betfair.StreamingAPI.Types.Field
import           Betfair.StreamingAPI.Types.MarketChange
import           Betfair.StreamingAPI.Types.MarketDataFilter
import           Betfair.StreamingAPI.Types.MarketDefinition
import           Betfair.StreamingAPI.Types.MarketFilter
import qualified Betfair.StreamingAPI.Types.MarketFilter                 as MF
import           Betfair.StreamingAPI.Types.MarketStatus
import           Betfair.StreamingAPI.Types.Order
import           Betfair.StreamingAPI.Types.OrderFilter
import           Betfair.StreamingAPI.Types.OrderMarketChange
import           Betfair.StreamingAPI.Types.OrderRunnerChange
import           Betfair.StreamingAPI.Types.OrderStatus
import           Betfair.StreamingAPI.Types.OrderType
import           Betfair.StreamingAPI.Types.PersistenceType
import           Betfair.StreamingAPI.Types.RequestStatus
import           Betfair.StreamingAPI.Types.RunnerChange
import           Betfair.StreamingAPI.Types.RunnerDefinition
import           Betfair.StreamingAPI.Types.RunnerStatus
import           Betfair.StreamingAPI.Types.SegmentType
import           Betfair.StreamingAPI.Types.Side

--
-- app key from betfair subscription
-- session token from the api
sampleStart :: AppKey -> SessionToken -> IO ()
sampleStart a stoken = void (stream a stoken)

stream :: AppKey -> SessionToken -> IO StreamingState
stream a = fmap cState . startStreaming . initializeContext a

startStreaming :: Context -> IO (Context)
startStreaming context =
  bracket
    connectToBetfair
    (\connection ->
       putStrLn "Closing connection" >> toLog context "Closing connection" >>
       connectionClose connection)
    (\connection ->
       (cOnConnection context) (context {cConnection = connection}) >>=
       authenticateAndReadDataLoop)

authenticateAndReadDataLoop :: Context -> IO (Context)
authenticateAndReadDataLoop c =
  sresponse c >>= authentication >>= sresponse >>= resendLastSubscription >>=
  readDataLoop
  where
    sresponse = fmap snd . response

readDataLoop :: Context -> IO (Context)
readDataLoop c = response c >>= reSubscribeIfNeeded >>= readDataLoop

connectToBetfair :: IO Connection
connectToBetfair =
  initConnectionContext >>=
  flip connectTo (ConnectionParams (cs host) port (Just def) Nothing)

host :: Text
-- for pre-production
host = "stream-api-integration.betfair.com"

-- for production
-- host = "stream-api.betfair.com"
port :: PortNumber
port = 443

reSubscribeIfNeeded :: (Maybe MarketSubscriptionMessage, Context)
                    -> IO (Context)
reSubscribeIfNeeded (Just m, c) = resubscribe m c
reSubscribeIfNeeded (_, c)      = return c

resendLastSubscription :: Context -> IO (Context)
resendLastSubscription c =
  (flip resubscribe c .
   fromMaybe (def :: MarketSubscriptionMessage) . lastMarketSubscriptionMessage)
    c
