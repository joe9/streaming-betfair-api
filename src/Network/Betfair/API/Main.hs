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
import Network.Betfair.API.NonBlockingReadFromTChan

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

application :: Context -> [MarketId] -> IO ()
application context [] =
  putStrLn "No Market Ids provided, hence, not connecting"
application context mids =
  do connection <- connectToBetfair
     (token,_,loggedOutput) <-
       runRWST applicationLoop
               context{cConnection = connection}
               (def :: StreamingState) {ssAppKey = cAppKey context
                                       ,ssConnectionState = NotAuthenticated
                                       ,ssSessionToken = "SESSIONTOKEN"
                                       ,ssMarkets =
                                          (Map.fromList .
                                           map (\mid ->
                                                  (mid,def {msMarketId = mid}))) mids}
--      putStrLn loggedOutput
     -- add the below close to signal handler, to close the connection on
     -- interrupt, etc
     log (cWriteLogChannel context) "Closing connection"
     connectionClose connection

applicationLoop
  :: RWST Context () StreamingState IO ()
applicationLoop =
  do response
     ss <- get
     checkAuthentication (ssConnectionState ss)
     _ <- response
     return ()
--      applicationLoop

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
