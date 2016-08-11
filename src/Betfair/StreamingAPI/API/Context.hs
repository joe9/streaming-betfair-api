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
           cWriteLog      :: Text -> IO ()
          ,cWriteState    :: StreamingState -> IO ()
          ,cConnection    :: Connection}

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
          ,cWriteLog = l
          ,cWriteState = st
          ,cConnection = undefined}-- ,cWriteResponses :: Either ResponseException Response -> IO ()
