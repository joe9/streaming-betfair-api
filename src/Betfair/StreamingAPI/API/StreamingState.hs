{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Betfair.StreamingAPI.API.StreamingState
  ( StreamingState(..)
  , defaultStreamingState
  , SessionToken
  ) where

import           Data.Aeson.TH                  (Options (omitNothingFields),
                                                 defaultOptions,
                                                 deriveJSON)
import qualified Data.IntMap.Strict             as IntMap
import           Data.Time
import           Protolude

import           Text.PrettyPrint.GenericPretty

import Betfair.APING

import Betfair.StreamingAPI.API.Request

type SessionToken = Token

data StreamingState = StreamingState
  { ssRequests                            :: IntMap.IntMap Request
  , ssIdCounter                           :: Int
  , ssSessionToken                        :: SessionToken
  , ssAppKey                              :: AppKey
  , ssNeedHumanHelp                       :: Bool
  , ssLastMarketSubscriptionMessageSentAt :: UTCTime
  } deriving (Eq, Read, Show, Generic, Pretty)

defaultStreamingState :: UTCTime -> StreamingState
defaultStreamingState time = StreamingState IntMap.empty 1 "" "" False time

$(deriveJSON defaultOptions {omitNothingFields = True} ''StreamingState)
