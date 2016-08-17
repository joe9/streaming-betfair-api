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
import           Control.Monad.Trans.Except
--
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Response
import Betfair.StreamingAPI.API.ResponseException
import Betfair.StreamingAPI.API.StreamingState

data Context a =
  Context {cBlockingReadMarketIds :: Context a -> ExceptT ResponseException IO ([MarketId],Context a)
          ,cNonBlockingReadMarketIds :: Context a -> ExceptT ResponseException IO ([MarketId],Context a)
          ,cLogger :: Text -> IO ()
          ,cOnResponse :: Response -> Context a -> ExceptT ResponseException IO (Context a)
          ,cConnection :: Connection
          ,cOnConnection :: Context a -> ExceptT ResponseException IO (Context a)
          ,cState :: StreamingState
          ,cUserState :: a
          }

initializeContext
  :: AppKey -> SessionToken -> Context a
initializeContext a s =
  Context {cBlockingReadMarketIds = (\c -> return([],c))
          ,cNonBlockingReadMarketIds = (\c -> return([],c))
          ,cLogger = putStrLn
          ,cOnResponse = (\r c -> lift (print r) >> return c)
          ,cConnection = undefined
          ,cOnConnection = (\c -> return c)
          ,cState =
             def {ssAppKey = a
                 ,ssSessionToken = s}
          ,cUserState = undefined
          }

instance Show (Context a) where
  show = cs . showContext

showContext :: Context a -> Text
showContext c = "Context: " <> BasicPrelude.show (cState c)-- <> ", " <> (cShowUserState c) (cUserState c)
