{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Betfair.StreamingAPI.API.StreamingState
  ( StreamingState(..)
  , timeInMicroseconds
  , defaultStreamingState
  ) where

import           Protolude
import           Data.Aeson.TH         (Options (omitNothingFields),
                                        defaultOptions, deriveJSON)
import qualified Data.IntMap.Strict    as IntMap
import           Data.Time.Clock.POSIX
import Data.Time.Units

--
import TimeUnitsJSONInstance()
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Request

data StreamingState = StreamingState
  { ssRequests                            :: IntMap.IntMap Request
  , ssIdCounter                           :: Int
  , ssSessionToken                        :: SessionToken
  , ssAppKey                              :: AppKey
  , ssNeedHumanHelp                       :: Bool
  , ssLastMarketSubscriptionMessageSentAt :: Microsecond
  } deriving (Eq, Read, Show)

defaultStreamingState :: StreamingState
defaultStreamingState = StreamingState IntMap.empty 1 "" "" False (fromMicroseconds 0)

$(deriveJSON defaultOptions {omitNothingFields = True} ''StreamingState)

-- to use for the ssLastMarketSubscriptionMessageSentAt
timeInMicroseconds :: IO Microsecond
timeInMicroseconds =
  fromMicroseconds . fromIntegral . numerator . toRational . (* 1000000) <$> getPOSIXTime
