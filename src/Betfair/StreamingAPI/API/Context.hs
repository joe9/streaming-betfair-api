{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Context
  (Context(..)
  ,initializeContext)
  where

import           BasicPrelude               hiding (show)
import qualified BasicPrelude
import           Data.Default
import           Data.String.Conversions
import           GHC.Show
import           Network.Connection
--
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Response
-- import Betfair.StreamingAPI.API.ResponseException
import Betfair.StreamingAPI.API.StreamingState

data Context a =
  Context {cConnection :: Connection
          ,cLogger :: Text -> IO ()
          ,cOnResponse :: Response -> Context a -> IO (Context a)
          ,cOnConnection :: Context a -> IO (Context a)
          ,cState :: StreamingState
          ,cUserState :: a}

-- Should I pass through the ResponsException to the cOnResponse?
initializeContext
  :: AppKey -> SessionToken -> Context a
initializeContext a s =
  Context {cConnection = undefined
          ,cLogger = putStrLn
          ,cOnResponse = \r c -> (print r) >> return c
          ,cOnConnection = return
          ,cState =
             def {ssAppKey = a
                 ,ssSessionToken = s}
          ,cUserState = undefined}

instance Show (Context a) where
  show = cs . showContext

showContext :: Context a -> Text
showContext c = "Context: " <> BasicPrelude.show (cState c)-- <> ", " <> (cShowUserState c) (cUserState c)
