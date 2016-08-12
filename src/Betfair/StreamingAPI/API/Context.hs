{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Context
  (Context(..)
  ,initializeContext1
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

data Context a =
  Context {cBlockingReadMarketIds :: IO [MarketId]
          ,cNonBlockingReadMarketIds :: IO [MarketId]
          ,cLogger :: Text -> IO ()
          ,cWriteResponses :: Either ResponseException Response -> IO ()
          ,cWriteState :: StreamingState -> IO ()
          ,cConnection :: Connection
          ,cState :: StreamingState
          ,cUserState :: Maybe a
          ,cShowUserState :: Maybe (a -> Text)}

initializeContext
  :: AppKey -> SessionToken -> Context a
initializeContext a s =
  Context {cBlockingReadMarketIds = (return [])
          ,cNonBlockingReadMarketIds = (return [])
          ,cLogger = putStrLn
          ,cWriteResponses = print
          ,cWriteState = print
          ,cConnection = undefined
          ,cState =
             def {ssAppKey = a ,ssSessionToken = s}
          ,cUserState = Nothing
          ,cShowUserState = Nothing}

initializeContext1 :: AppKey
                   -> SessionToken
                   -> Maybe StreamingState
                   -> Maybe (IO [MarketId])
                   -> Maybe (IO [MarketId])
                   -> Maybe (Text -> IO ())
                   -> Maybe (Either ResponseException Response -> IO ())
                   -> Maybe (StreamingState -> IO ())
                   -> Maybe a
                   -> Maybe (a -> Text)
                   -> Context a
initializeContext1 a s mss mb mn l r st userState showUserState =
  Context {cBlockingReadMarketIds = fromMaybe (return []) mb
          ,cNonBlockingReadMarketIds = fromMaybe (return []) mn
          ,cLogger = fromMaybe putStrLn l
          ,cWriteResponses = fromMaybe print r
          ,cWriteState = fromMaybe print st
          ,cConnection = undefined
          ,cState =
             (fromMaybe def mss) {ssAppKey = a
                                 ,ssSessionToken = s}
          ,cUserState = userState
          ,cShowUserState = showUserState}

instance Show (Context a) where
  show = cs . showContext

showContext :: Context a -> Text
showContext c = "Context: " <> BasicPrelude.show (cState c) -- <> ", " <> (cShowUserState c) (cUserState c)
