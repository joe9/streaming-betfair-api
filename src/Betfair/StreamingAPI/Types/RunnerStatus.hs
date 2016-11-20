{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.RunnerStatus
  ( RunnerStatus(..)
  , defaultRunnerStatus
  ) where

import Text.PrettyPrint.GenericPretty
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
  deriving (Eq, Show, Generic, Pretty, Read, Enum)

defaultRunnerStatus :: RunnerStatus
defaultRunnerStatus = ACTIVE

$(deriveJSON defaultOptions {omitNothingFields = True} ''RunnerStatus)
