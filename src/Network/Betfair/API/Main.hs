{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Main
  (main
  ,application)
  where

import           Control.Monad.RWS
import           Data.Aeson
import           Data.Default
import qualified Data.Map.Strict    as Map
import           Network.Connection
import           Network.Connection
import           Network.Socket

import WriterLog
import WriterLog

import Network.Betfair.API.CommonTypes
import Network.Betfair.API.Config
import Network.Betfair.API.Request
import Network.Betfair.API.RequestProcessing
import Network.Betfair.API.Response
import Network.Betfair.API.StreamingState
import Network.Betfair.Responses.ConnectionMessage
import Network.Betfair.Responses.MarketChangeMessage
import Network.Betfair.Responses.OrderChangeMessage
import Network.Betfair.Responses.StatusMessage

main :: IO ()
main =
  application (def {appKey = "testappkey"})
              ["1.125402056"]

application :: Config -> [MarketId] -> IO ()
application config [] =
  putStrLn "No Market Ids provided, hence, not connecting"
application config mids =
  do connection <- connectToBetfair
     (token,_,loggedOutput) <-
       runRWST applicationLoop
               connection
               (def :: StreamingState) {ssConfig = config
                                       ,ssConnectionState = NotAuthenticated
                                       ,ssSessionToken = "SESSIONTOKEN"
                                       ,ssMarkets =
                                          (Map.fromList .
                                           map (\mid ->
                                                  (mid,def {msMarketId = mid}))) mids}
--      putStrLn loggedOutput
     -- add the below close to signal handler, to close the connection on
     -- interrupt, etc
     putStrLn "Closing connection"
     connectionClose connection

applicationLoop
  :: RWST Connection Log StreamingState IO ()
applicationLoop =
  do response >>= (\c -> lift (putStr "--->") >> (lift . print) c)
     ss <- get
     checkAuthentication (ssConnectionState ss)
     response >>= (\c -> lift (putStr "--->") >> (lift . print) c)
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
  :: ConnectionState -> RWST Connection Log StreamingState IO ()
checkAuthentication NotAuthenticated =
  do authentication
     s <- get
     put (s {ssConnectionState = AuthenticateSent})
checkAuthentication _ = return ()
