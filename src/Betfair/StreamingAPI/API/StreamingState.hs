{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Betfair.StreamingAPI.API.StreamingState
  ( StreamingState(..)
  , timeInMicroseconds
  , defaultStreamingState
  ) where

import           Data.Aeson.TH         (Options (omitNothingFields),
                                        defaultOptions, deriveJSON)
import qualified Data.IntMap.Strict    as IntMap
import           Data.Time.Clock.POSIX
import           Data.Time.Units
import           Data.Time
import           Protolude

--
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Request
import TimeUnitsJSONInstance                ()

data StreamingState = StreamingState
  { ssRequests                            :: IntMap.IntMap Request
  , ssIdCounter                           :: Int
  , ssSessionToken                        :: SessionToken
  , ssAppKey                              :: AppKey
  , ssNeedHumanHelp                       :: Bool
  , ssLastMarketSubscriptionMessageSentAt :: UTCTime
  } deriving (Eq, Read, Show)

defaultStreamingState :: UTCTime -> StreamingState
defaultStreamingState time =
  StreamingState IntMap.empty 1 "" "" False time

$(deriveJSON defaultOptions {omitNothingFields = True} ''StreamingState)

-- to use for the ssLastMarketSubscriptionMessageSentAt
timeInMicroseconds :: IO Microsecond
timeInMicroseconds =
  fromMicroseconds . fromIntegral . numerator . toRational . (* 1000000) <$>
  getPOSIXTime
