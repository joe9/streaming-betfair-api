{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.BettingType
  ( BettingType(..)
  , defaultBettingType
  ) where

import Text.PrettyPrint.GenericPretty
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Protolude

data BettingType
  = ODDS
  | LINE
  | RANGE
  | ASIAN_HANDICAP_DOUBLE_LINE
  | ASIAN_HANDICAP_SINGLE_LINE
  | FIXED_ODDS
  deriving (Eq, Read, Show, Generic, Pretty)

$(deriveJSON defaultOptions {omitNothingFields = True} ''BettingType)

defaultBettingType :: BettingType
defaultBettingType = ODDS
