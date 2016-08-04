{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Main
  (main
  ,application)
  where

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import           Control.Monad.RWS
import           Data.Aeson
import           Data.Default
import qualified Data.Map.Strict    as Map
import           Network.Connection
import           Network.Connection
import           Network.Socket

import Prelude hiding (log)
import Network.Betfair.API.CommonTypes
import Network.Betfair.API.Log
import Network.Betfair.API.Config
import Network.Betfair.API.Context
import Network.Betfair.API.Request
import Network.Betfair.API.RequestProcessing
import Network.Betfair.API.Response
import Network.Betfair.API.StreamingState
import Network.Betfair.Responses.ConnectionMessage
import Network.Betfair.Responses.MarketChangeMessage
import Network.Betfair.Responses.OrderChangeMessage
import Network.Betfair.Responses.StatusMessage
import Network.Betfair.API.ReadFromTChan

main :: IO ()
main = do
  context <- initializeContext "testappkey"
  logReader <- forkIO ( readerThread (cWriteLogChannel context))
  application context ["1.125402056"]

readerThread :: TChan String -> IO ()
readerThread chan = do
        newInt <- atomically $ readTChan chan
        putStrLn $ "read new value: "
        putStrLn newInt
        readerThread chan

entry
  betfairProcessingLoop
    (def :: StreamingState) {ssAppKey = cAppKey context
                            ,ssConnectionState = NotAuthenticated
                            ,ssSessionToken = "SESSIONTOKEN"}

betfairProcessingLoop :: Context -> StreamingState -> IO ()
betfairProcessingLoop context ss =
--   blocking read for MarketId's
  mids <- readMarketIdsFromTChan (cReadMarketIdsChannel context))
--   start processing those marketids and market ids from streaming state
  newSs <- betfairProcessing ( ss {ssMarkets = (Map.fromList . map (\mid -> (mid,def {msMarketId = mid}))) mids})
  when newState.ssConnectionState /= AuthenticationFailed
            betfairProcessingLoop context ss

betfairProcessing :: Context -> [MarketId] -> IO ()
betfairProcessing context [] =
  putStrLn "No Market Ids provided, hence, not connecting"
betfairProcessing ss
--  if there are no market ids to process, get out
  | ssMarkets ss == null = return ss
-- start processing if there are any marketid's in streaming state
  | otherwise =
     result <- try connectToBetfair :: IO (Either SomeException Connection)
     case result of
       Left ex -> putStrLn ("Caught exception" ++ show ex) >> threadDelayInSeconds 60 >> betfarProcessing ss
       Right connection -> do
        newState <- finally (fmap (\(_,s,_) -> s)
                              (runRWST authenticateAndReadDataLoop
                                    context{cConnection = connection} ss))
                           (log (cWriteLogChannel context) "Closing connection"
                              >> connectionClose connection)
        when newState.ssConnectionState /= AuthenticationFailed
             betfairProcessing newState

authenticateAndReadDataLoop :: RWST Context () StreamingState IO ()
authenticateAndReadDataLoop =
updateDataloop : non blocking read for additional market id's
  if all markets are closed, get out
  do response
     ss <- get
     checkAuthentication (ssConnectionState ss)
     -- check status and ensure that the authentication was successful
     _ <- response
     if authenticated, go to readData loop
     else error out
     return ()
--      applicationLoop

readDataLoop :: RWST Context () StreamingState IO ()
readDataLoop =
  if all markets are closed, get out
  do
     updateDataloop : non blocking read for additional market id's
     write state if changed
     send subscribe requests if needed
     response
     write response to cWriteResponseChannel
     readDataLoop

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

initialStreamState :: StreamingState
initialStreamState = def

checkAuthentication
  :: ConnectionState -> RWST Context () StreamingState IO ()
checkAuthentication NotAuthenticated =
  do authentication
     s <- get
     put (s {ssConnectionState = AuthenticateSent})
checkAuthentication _ = return ()
