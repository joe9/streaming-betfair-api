{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.BettingType
  ( BettingType(..)
  , defaultBettingType
  ) where

import Protolude
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)

data BettingType
  = ODDS
  | LINE
  | RANGE
  | ASIAN_HANDICAP_DOUBLE_LINE
  | ASIAN_HANDICAP_SINGLE_LINE
  | FIXED_ODDS
  deriving (Eq, Read, Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''BettingType)

defaultBettingType :: BettingType
defaultBettingType = ODDS
