{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Context
  (Context(..)
  ,initializeContext)
  where

import           BasicPrelude            hiding (show)
import qualified BasicPrelude
import           Data.Default
import           Data.String.Conversions
import           GHC.Show
import           Network.Connection
--
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Response
import Betfair.StreamingAPI.API.ResponseException
import Betfair.StreamingAPI.API.StreamingState

data Context =
  Context {cReadMarketIds :: IO [MarketId]
          ,cLogger :: Text -> IO ()
          ,cWriteResponses :: Either ResponseException Response -> IO ()
          ,cWriteState :: StreamingState -> IO ()
          ,cConnection :: Connection
          ,cState :: StreamingState}

initializeContext :: AppKey
                  -> SessionToken
                  -> IO [MarketId]
                  -> (Text -> IO ())
                  -> (Either ResponseException Response -> IO ())
                  -> (StreamingState -> IO ())
                  -> Context
initializeContext a s m l r st =
  Context {cReadMarketIds = m
          ,cLogger = l
          ,cWriteResponses = r
          ,cWriteState = st
          ,cConnection = undefined
          ,cState =
             def {ssAppKey = a
                 ,ssSessionToken = s}}

instance Show Context where
  show = cs . showContext

showContext :: Context -> Text
showContext c = "Context: " <> BasicPrelude.show (cState c)
