{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Context
  (Context(..)
  ,initializeContext)
  where

import BasicPrelude
import Betfair.StreamingAPI.API.CommonTypes
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Data.Text
import Network.Connection

data Context =
  Context {cAppKey :: AppKey
          ,cReadMarketIdsChannel :: TChan Text
          ,cWriteResponsesChannel :: TChan Text
          ,cWriteLogChannel :: TChan Text
          ,cWriteStateChannel :: TChan Text
          ,cSessionToken :: SessionToken
          ,cConnection :: Connection}

initializeContext
  :: AppKey -> SessionToken -> IO Context
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
