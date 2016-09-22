{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Betfair.StreamingAPI.API.StreamingState
  ( StreamingState(..)
  , timeInMicroseconds
  ) where

import           BasicPrelude
import           Data.Aeson.TH         (Options (omitNothingFields),
                                        defaultOptions, deriveJSON)
import           Data.Default
import qualified Data.IntMap.Strict    as IntMap
import           Data.Ratio
import           Data.Time.Clock.POSIX

--
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Request

data StreamingState = StreamingState
  { ssRequests                            :: IntMap.IntMap Request
  , ssIdCounter                           :: Int
  , ssSessionToken                        :: SessionToken
  , ssAppKey                              :: AppKey
  , ssNeedHumanHelp                       :: Bool
  , ssLastMarketSubscriptionMessageSentAt :: Integer
  } deriving (Eq, Read, Show)

instance Default StreamingState where
  def = StreamingState IntMap.empty 1 "" "" False 0

-- deriveDefault ''MarketDefinition
$(deriveJSON defaultOptions {omitNothingFields = True} ''StreamingState)

-- to use for the ssLastMarketSubscriptionMessageSentAt
timeInMicroseconds :: IO Integer
timeInMicroseconds =
  fromIntegral . numerator . toRational . (* 1000000) <$> getPOSIXTime
