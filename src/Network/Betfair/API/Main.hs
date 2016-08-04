{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Main
  (main
  ,start
  ,startStreaming
  ,streamMarketIds)
  where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad.RWS
import           Control.Monad.STM
import           Data.Aeson
import           Data.Maybe
import           Data.Default
import qualified Data.Map.Strict              as Map
import           Network.Connection
import           Network.Connection
import           Network.Socket

import Network.Betfair.API.CommonTypes
import Network.Betfair.API.Config
import Network.Betfair.API.Context
import Network.Betfair.API.Log
import Network.Betfair.API.ReadFromTChan
import Network.Betfair.API.Request
import Network.Betfair.API.RequestProcessing
import Network.Betfair.API.Response
import Network.Betfair.API.Response
import Network.Betfair.API.StreamingState
import Network.Betfair.Responses.ConnectionMessage
import Network.Betfair.Responses.MarketChangeMessage
import Network.Betfair.Responses.OrderChangeMessage
import Network.Betfair.Responses.StatusMessage
import Prelude                                       hiding (log)

main :: IO ()
main = start "appkey"

start :: AppKey -> IO ()
start appKey =
  do context <- initializeContext appKey
     logReader <- forkIO (readerThread (cWriteLogChannel context))
     _ <- startStreaming context
         (Just def {ssMarkets =
                    (Map.fromList . map (\mid -> (mid,def {msMarketId = mid}))) ["1.125402056"]})
     return ()

readerThread :: TChan String -> IO ()
readerThread chan =
  do newInt <- atomically $ readTChan chan
     putStrLn $ "read new value: "
     putStrLn newInt
     readerThread chan

-- if you do not have old state, call this function
startStreaming :: Context -> Maybe StreamingState -> IO StreamingState
startStreaming context =
  streamMarketIds
    context . (\ss -> ss {ssAppKey = cAppKey context
                            ,ssConnectionState = NotAuthenticated
                            ,ssSessionToken = "SESSIONTOKEN"})
    . fromMaybe (def :: StreamingState)

streamMarketIds
  :: Context -> StreamingState -> IO StreamingState
--  if there are no market ids to process, get out
streamMarketIds context ss
  | null (ssMarkets ss) =
   do
     -- blocking read for MarketId's, waiting for marketIds as there
     -- are none to stream
     mids <- readMarketIdsFromTChan (cReadMarketIdsChannel context)
     -- start processing those marketids and market ids from streaming state
     streamMarketIds
         context
         (ss {ssMarkets =
                (Map.fromList . map (\mid -> (mid,def {msMarketId = mid}))) mids})
  |
   -- start processing if there are any marketid's in streaming state
   otherwise =
    do result <- try connectToBetfair :: IO (Either SomeException Connection)
       case result of
         Left ex ->
           log (cWriteLogChannel context)
               ("streamMarketIds: Caught exception: " ++ show ex) >>
           threadDelay (60 * 1000 * 1000) >>
           streamMarketIds context ss
         Right connection ->
           do newState <-
                finally (fmap (\(_,s,_) -> s)
                              (runRWST authenticateAndReadDataLoop
                                       context {cConnection = connection}
                                       ss))
                        (log (cWriteLogChannel context) "Closing connection" >>
                         connectionClose connection)
              if (ssNeedHumanHelp newState)
                 then return newState
                 else streamMarketIds context newState

authenticateAndReadDataLoop
  :: RWST Context () StreamingState IO ()
authenticateAndReadDataLoop =
  do response
     ss <- get
     checkAuthentication (ssConnectionState ss)
     -- check status and ensure that the authentication was successful
     r <- response
     ssa <- get
     let ssb = ssa {ssNeedHumanHelp = isHumanHelpNeeded r}
     put ssb
     when (not (isHumanHelpNeeded r)) readDataLoop

readDataLoop
  :: RWST Context () StreamingState IO ()
readDataLoop =
  do ss <- get
     -- TODO if all markets are closed, get out
     if (null (ssMarkets ss))
        then return ()
        else do mids <- nonBlockingReadMarketIds
                -- write state if changed
                -- send subscribe requests if needed
                put
                  (ss {ssMarkets =
                         (Map.fromList .
                          map (\mid -> (mid,def {msMarketId = mid}))) mids})
                r <- response
                context <- ask
                lift (atomically
                        (writeTChan (cWriteResponsesChannel context)
                                    (show r)))
                ssa <- get
                let ssb = ssa {ssNeedHumanHelp = isHumanHelpNeeded r}
                put ssb
                if (ssNeedHumanHelp ssb)
                   then return ()
                   else readDataLoop

connectToBetfair :: IO Connection
connectToBetfair =
  initConnectionContext >>=
  flip connectTo
       (ConnectionParams host
                         port
                         (Just def)
                         Nothing)

host :: String
-- for pre-production
host = "stream-api-integration.betfair.com"
-- for production
-- host = "stream-api.betfair.com"

port :: PortNumber

port = 443

checkAuthentication
  :: ConnectionState -> RWST Context () StreamingState IO ()
checkAuthentication NotAuthenticated =
  do authentication
     s <- get
     put (s {ssConnectionState = AuthenticateSent})
checkAuthentication _ = return ()

isHumanHelpNeeded :: Response -> Bool
isHumanHelpNeeded _ = True
