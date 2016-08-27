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
import Betfair.StreamingAPI.Requests.MarketSubscriptionMessage
-- import Betfair.StreamingAPI.API.ResponseException
import Betfair.StreamingAPI.API.StreamingState

data Context =
  Context {cConnection   :: Connection
          ,cLogger       :: Text -> IO ()
          ,cOnResponse   :: Response -> Context -> IO (Maybe MarketSubscriptionMessage,Context)
          ,cOnConnection :: Context -> IO (Context)
          ,cState        :: StreamingState
          }

-- Should I pass through the ResponsException to the cOnResponse?
initializeContext
  :: AppKey -> SessionToken -> Context
initializeContext a s =
  Context {cConnection = undefined
          ,cLogger = putStrLn
          ,cOnResponse = \r c -> print r >> return (Nothing,c)
          ,cOnConnection = return
          ,cState =
             def {ssAppKey = a
                 ,ssSessionToken = s}
          }

instance Show (Context) where
  show = cs . showContext

showContext :: Context -> Text
showContext c = "Context: " <> BasicPrelude.show (cState c)-- <> ", " <> (cShowUserState c) (cUserState c)
