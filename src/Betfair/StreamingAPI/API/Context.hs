{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Context
  (Context(..)
  ,initializeContext)
  where

import BasicPrelude
import Network.Connection
--
-- import Betfair.StreamingAPI
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.StreamingState

data Context =
  Context {cAppKey        :: AppKey
          ,cReadMarketIds :: IO [MarketId]
          ,cSessionToken  :: SessionToken
          ,
           --           ,cWriteResponses :: Either ResponseException Response -> IO ()
           cLogger      :: Text -> IO ()
          ,cWriteState    :: StreamingState -> IO ()
          ,cConnection    :: Connection
          ,cState    :: StreamingState
          }

initializeContext :: AppKey
                  -> SessionToken
                  -> IO [MarketId]
                  -> (Text -> IO ())
                  -> (StreamingState -> IO ())
                  -> Context
initializeContext a s m l st =
  Context {cAppKey = a
          ,cReadMarketIds = m
          ,cSessionToken = s
          ,cLogger = l
          ,cWriteState = st
          ,cConnection = undefined
          ,cState = def}-- ,cWriteResponses :: Either ResponseException Response -> IO ()
