{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Betfair.StreamingAPI
  (
   -- from this file
   streamMarketIds
  ,sampleStart
  ,
   -- Common Types
   MarketId
  ,MarketName
  ,EventName
  ,SessionToken
  ,AppKey
  ,
   -- Other Types explicitly defined in modules
   PersistenceType
  ,Order
  ,OrderMarketChange
  ,ChangeType
  ,MarketFilter
  ,MarketChange
  ,OrderRunnerChange
  ,Field
  ,MarketDefinition
  ,OrderFilter
  ,Side
  ,RequestStatus
  ,RunnerChange
  ,OrderType
  ,ErrorCode
  ,BettingType
  ,RunnerDefinition
  ,OrderStatus
  ,SegmentType
  ,MarketStatus
  ,MarketDataFilter
  ,RunnerStatus)
  where

import BasicPrelude hiding (finally)
--
import Betfair.StreamingAPI.API.AddId
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Config
import Betfair.StreamingAPI.API.Context
import Betfair.StreamingAPI.API.Log
-- import Betfair.StreamingAPI.API.ReadFromTChan
import Betfair.StreamingAPI.API.Request
import Betfair.StreamingAPI.API.RequestProcessing
import Betfair.StreamingAPI.API.Response
import Betfair.StreamingAPI.API.ResponseException
import Betfair.StreamingAPI.API.ResponseProcessing
import Betfair.StreamingAPI.API.StreamingState
import Betfair.StreamingAPI.Requests.AuthenticationMessage
import Betfair.StreamingAPI.Requests.HeartbeatMessage
import Betfair.StreamingAPI.Requests.MarketSubscriptionMessage
import Betfair.StreamingAPI.Requests.OrderSubscriptionMessage
import Betfair.StreamingAPI.Responses.ConnectionMessage
import Betfair.StreamingAPI.Responses.MarketChangeMessage
import Betfair.StreamingAPI.Responses.OrderChangeMessage
import Betfair.StreamingAPI.Responses.StatusMessage
import Betfair.StreamingAPI.Types.BettingType
import Betfair.StreamingAPI.Types.ChangeType
import Betfair.StreamingAPI.Types.ErrorCode
import Betfair.StreamingAPI.Types.Field
import Betfair.StreamingAPI.Types.MarketChange
import Betfair.StreamingAPI.Types.MarketDataFilter
import Betfair.StreamingAPI.Types.MarketDefinition
import Betfair.StreamingAPI.Types.MarketFilter
import Betfair.StreamingAPI.Types.MarketStatus
import Betfair.StreamingAPI.Types.Order
import Betfair.StreamingAPI.Types.OrderFilter
import Betfair.StreamingAPI.Types.OrderMarketChange
import Betfair.StreamingAPI.Types.OrderRunnerChange
import Betfair.StreamingAPI.Types.OrderStatus
import Betfair.StreamingAPI.Types.OrderType
import Betfair.StreamingAPI.Types.PersistenceType
import Betfair.StreamingAPI.Types.RequestStatus
import Betfair.StreamingAPI.Types.RunnerChange
import Betfair.StreamingAPI.Types.RunnerDefinition
import Betfair.StreamingAPI.Types.RunnerStatus
import Betfair.StreamingAPI.Types.SegmentType
import Betfair.StreamingAPI.Types.Side
--
import           Control.Concurrent
import           Control.Exception.Safe
import           Control.Monad.Trans.Except
import           Data.Default
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                  hiding (map, null)
import           Data.Text.IO
import           Network.Connection
import           Network.Socket

-- app key from betfair subscription
-- session token from the api
sampleStart
  :: AppKey -> SessionToken -> [MarketId] -> IO ()
sampleStart a stoken mids =
  streamMarketIds a stoken mids Nothing Nothing Nothing Nothing Nothing Nothing >>
  return ()

streamMarketIds :: AppKey
                -> SessionToken
                -> [MarketId]
                -> Maybe StreamingState
                -> Maybe (IO [MarketId])
                -> Maybe (IO [MarketId])
                -> Maybe (Text -> IO ())
                -> Maybe (Either ResponseException Response -> IO ())
                -> Maybe (StreamingState -> IO ())
                -> IO StreamingState
streamMarketIds a stoken mids mss m mn l r st =
  (fmap cState . startStreaming)
    (context {cState =
                addMarketIds (cState context)
                             mids})
  where
        --   context = initializeContext a stoken mss m mn l r st
        context = initializeContext a stoken

startStreaming :: (Context a) -> IO (Context a)
startStreaming context
  | null (ssMarkets (cState context)) =
    do
       -- blocking read for MarketId's, waiting for marketIds as there
       -- are none to stream
       mids <- cBlockingReadMarketIds context
       -- start processing those marketids and market ids from streaming state
       startStreaming
         (context {cState =
                     addMarketIds (cState context)
                                  mids})
  |
   -- start processing if there are any marketid's in streaming state
   -- http://learnyouahaskell.com/input-and-output#exceptions
   -- https://haskell-lang.org/tutorial/exception-safety
   -- https://haskell-lang.org/library/safe-exceptions
   -- http://neilmitchell.blogspot.com/2015/05/handling-control-c-in-haskell.html
   -- the below exception handling mechanism is perfect. "tryAny"
   -- handles any synchronous exceptions and recovers from
   -- them. Synchronous exceptions are exceptions directly related to
   -- the executed code such as "no network connection", "no host",
   -- etc. Whereas, "finally" is for cleanup. "finally" handles both
   -- synchronous and asynchronous exceptions. If a user presses
   -- Ctrl-C (asynchronous exception), "finally" cleans up the open
   -- connection thus preventing a leak.
   otherwise =
    do result <- tryAny connectToBetfair
       case result of
         Left err ->
           toLog context ("streamMarketIds: Caught exception: " <> show err) >>
           threadDelay (60 * 1000 * 1000) >>
           startStreaming context
         Right connection ->
           do eitherContext <-
                finally (runExceptT
                           (authenticateAndReadDataLoop
                              context {cConnection = connection}))
                        (toLog context "Closing connection" >>
                         connectionClose connection)
              case eitherContext of
                Left e ->
                  stdOutAndLog context
                               None
                               (show e) >>
                  return context
                Right r -> startStreaming r

authenticateAndReadDataLoop
  :: (Context a) -> ExceptT ResponseException IO (Context a)
authenticateAndReadDataLoop c =
  responseT c >>= lift . authentication . snd >>= responseT >>=
  (\(_,cu) ->
     (lift . marketIdsSubscription cu) ((Map.keys . ssMarkets . cState) cu)) >>=
  readDataLoop

readDataLoop
  :: (Context a) -> ExceptT ResponseException IO (Context a)
-- readDataLoop c = undefined
readDataLoop c
-- TODO if all markets are closed, get out
  | (null . ssMarkets . cState) c = return c
  | otherwise =
    do mids <- lift (cNonBlockingReadMarketIds c)
       -- write state if changed
       -- send subscribe requests to new markets only, if needed
       let cu =
             c {cState =
                  addMarketIds (cState c)
                               mids}
       responseT cu >>= readDataLoop . snd

connectToBetfair :: IO Connection
connectToBetfair =
  initConnectionContext >>=
  flip connectTo
       (ConnectionParams (cs host)
                         port
                         (Just def)
                         Nothing)

host :: Text
-- for pre-production
host = "stream-api-integration.betfair.com"

-- for production
-- host = "stream-api.betfair.com"
port :: PortNumber
port = 443
