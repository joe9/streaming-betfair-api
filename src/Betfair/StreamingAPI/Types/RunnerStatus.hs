{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.RunnerStatus
  ( RunnerStatus(..)
  , defaultRunnerStatus
  ) where

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Protolude

data RunnerStatus
  = ACTIVE
  | WINNER
  | LOSER
  | REMOVED_VACANT
  | REMOVED
  | HIDDEN
  deriving (Eq, Show, Read, Enum)

defaultRunnerStatus :: RunnerStatus
defaultRunnerStatus = ACTIVE

$(deriveJSON defaultOptions {omitNothingFields = True} ''RunnerStatus)
