{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell      #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Betfair.StreamingAPI.API.StreamingState
  (StreamingState(..)
  ,ConnectionState(..))
  where

import           BasicPrelude
import           Data.Default
import qualified Data.Map.Strict as Map
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
--
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Request

data StreamingState =
  StreamingState {ssRequests        :: Map.Map Integer Request
                 ,ssIdCounter       :: Integer
                 ,ssSessionToken    :: SessionToken
                 ,ssAppKey          :: AppKey
                 ,ssConnectionState :: ConnectionState
                 ,ssNeedHumanHelp   :: Bool}
  deriving (Eq,Read,Show)

instance Default StreamingState where
  def = StreamingState Map.empty 1 "" "" NotConnected False

data ConnectionState
  = NotConnected
  | NotAuthenticated
  | AuthenticateSent
  | Authenticated
  | ReceivingData
  deriving (Eq,Read,Show)

-- deriveDefault ''MarketDefinition
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''StreamingState)
