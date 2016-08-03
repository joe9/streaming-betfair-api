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
          ,cConnection :: Connection}

initializeContext :: AppKey -> IO Context
initializeContext a =
  do readMarketIdsChannel <- atomically $ newTChan
     writeResponsesChannel <- atomically $ newTChan
     writeLogChannel <- atomically $ newTChan
     writeStateChannel <- atomically $ newTChan
     return Context {cAppKey = a
                    ,cReadMarketIdsChannel = readMarketIdsChannel
                    ,cWriteResponsesChannel = writeResponsesChannel
                    ,cWriteLogChannel = writeLogChannel
                    ,cWriteStateChannel = writeStateChannel
                    ,cConnection = undefined}
