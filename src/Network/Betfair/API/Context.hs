{-# OPTIONS_GHC -Wall       #-}

module Network.Betfair.API.Context
  (Context(..)
  ,initializeContext)
  where

import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Network.Connection

import Network.Betfair.API.CommonTypes

data Context =
  Context {cAppKey                :: AppKey
          ,cReadMarketIdsChannel  :: TChan String
          ,cWriteResponsesChannel :: TChan String
          ,cWriteLogChannel       :: TChan String
          ,cWriteStateChannel     :: TChan String
          ,cSessionToken     :: SessionToken
          ,cConnection :: Connection}

initializeContext :: AppKey -> SessionToken -> IO Context
initializeContext a s =
  do readMarketIdsChannel <- atomically $ newTChan
     writeResponsesChannel <- atomically $ newTChan
     writeLogChannel <- atomically $ newTChan
     writeStateChannel <- atomically $ newTChan
     return Context {cAppKey = a
                    ,cReadMarketIdsChannel = readMarketIdsChannel
                    ,cWriteResponsesChannel = writeResponsesChannel
                    ,cWriteLogChannel = writeLogChannel
                    ,cWriteStateChannel = writeStateChannel
                    ,cSessionToken = s
                    ,cConnection = undefined}
