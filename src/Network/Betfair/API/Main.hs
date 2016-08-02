{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts  #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Main ( application ) where

import Network.Connection
import Control.Monad.RWS
import Data.Aeson
import Data.Default
import Network.Connection
import Network.Socket

import Network.Betfair.API.Request
import Network.Betfair.API.Response
import Network.Betfair.Responses.MarketChangeMessage
import Network.Betfair.Responses.OrderChangeMessage
import Network.Betfair.Responses.ConnectionMessage
import Network.Betfair.Responses.StatusMessage

type AppKey = String
type MarketId = String

data Config = Config { username      :: String
                     , password      :: String
                     , appKey        :: AppKey
                     , delayedAppKey :: AppKey
                     , onMarketChangeMessage :: MarketChangeMessage -> IO [MarketId]
                     , onOrderChangeMessage :: OrderChangeMessage -> IO [MarketId]
                     , onConnectionMessage :: ConnectionChangeMessage -> IO ()
                     , onStatusMessage :: StatusChangeMessage -> IO ()
                     , logger :: String -> IO ()
                     }

instance Default Config where
  def = Config "" "" "" "" print print print print

application :: Config -> [MarketId] -> IO ()
application config [] = print "No Market Ids provided, hence, not connecting"
application config mids =
  conn <- connectToBetfair
  (token,_,loggedOutput) <- runRWST response conn initialStreamState
  -- add the below close to signal handler, to close the connection on
         -- interrupt, etc
  connectionClose conn

-- applicationLoop :: RWST Config Log StreamingState IO ()
-- applicationLoop = do
--  applicationLoop

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
-- url = "stream-api.betfair.com"

port :: PortNumber
port = 443

initialStreamState :: StreamingState
initialStreamState = undefined
