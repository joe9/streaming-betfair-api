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
  Context {cBlockingReadMarketIds :: IO [MarketId]
          ,cNonBlockingReadMarketIds :: IO [MarketId]
          ,cLogger :: Text -> IO ()
          ,cWriteResponses :: Either ResponseException Response -> IO ()
          ,cWriteState :: StreamingState -> IO ()
          ,cConnection :: Connection
          ,cState :: StreamingState}

initializeContext :: AppKey
                  -> SessionToken
                  -> Maybe StreamingState
                  -> Maybe ( IO [MarketId])
                  -> Maybe ( IO [MarketId])
                  -> Maybe (Text -> IO ())
                  -> Maybe (Either ResponseException Response -> IO ())
                  -> Maybe (StreamingState -> IO ())
                  -> Context
initializeContext a s mss mb mn l r st =
  Context {cBlockingReadMarketIds = fromMaybe (return []) mb
          ,cNonBlockingReadMarketIds = fromMaybe (return []) mn
          ,cLogger = fromMaybe putStrLn l
          ,cWriteResponses = fromMaybe print r
          ,cWriteState = fromMaybe print st
          ,cConnection = undefined
          ,cState =
             (fromMaybe def mss) {ssAppKey = a
                                 ,ssSessionToken = s}}

instance Show Context where
  show = cs . showContext

showContext :: Context -> Text
showContext c = "Context: " <> BasicPrelude.show (cState c)
