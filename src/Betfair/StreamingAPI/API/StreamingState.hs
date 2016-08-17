{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell      #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Betfair.StreamingAPI.API.StreamingState
  (StreamingState(..)
  )
  where

import           BasicPrelude
import           Data.Default
import qualified Data.IntMap.Strict as IntMap
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
--
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Request

data StreamingState =
  StreamingState {ssRequests        :: IntMap.IntMap Request
                 ,ssIdCounter       :: Int
                 ,ssSessionToken    :: SessionToken
                 ,ssAppKey          :: AppKey
                 ,ssNeedHumanHelp   :: Bool}
  deriving (Eq,Read,Show)

instance Default StreamingState where
  def = StreamingState IntMap.empty 1 "" "" False

-- deriveDefault ''MarketDefinition
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''StreamingState)
