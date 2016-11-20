{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Context
  ( Context(..)
  , initializeContext
  ) where

import Text.PrettyPrint.GenericPretty
import           Data.String.Conversions (cs)
import           Data.Time
import           GHC.Show
import           Network.Connection
import           Protolude               hiding (show)
import qualified Protolude

import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Response
import Betfair.StreamingAPI.Requests.MarketSubscriptionMessage

-- import Betfair.StreamingAPI.API.ResponseException
import Betfair.StreamingAPI.API.StreamingState

data Context = Context
  { cConnection :: Connection
  , cLogger :: Text -> IO ()
  , cOnResponse :: ByteString -> Response -> Context -> IO (Maybe MarketSubscriptionMessage, Context)
  , cOnConnection :: Context -> IO (Context)
  , cState :: StreamingState
  }

-- Should I pass through the ResponsException to the cOnResponse?
initializeContext :: AppKey -> SessionToken -> UTCTime -> Context
initializeContext a s t =
  Context
  { cConnection = undefined
  , cLogger = putStrLn
  , cOnResponse = \_ r c -> print r >> return (Nothing, c)
  , cOnConnection = return
  , cState = (defaultStreamingState t) {ssAppKey = a, ssSessionToken = s}
  }

instance Show (Context) where
  show = cs . showContext

showContext :: Context -> Text
showContext c = "Context: " <> Protolude.show (cState c) -- <> ", " <> (cShowUserState c) (cUserState c)
