{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.RunnerStatus
  ( RunnerStatus(..)
  , defaultRunnerStatus
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

data RunnerStatus
  = ACTIVE
  | WINNER
  | LOSER
  | REMOVED
  | REMOVED_VACANT
  | HIDDEN
  | PLACED
  deriving (Eq, Show, Generic, Pretty, Read, Enum)

defaultRunnerStatus :: RunnerStatus
defaultRunnerStatus = ACTIVE

$(deriveJSON defaultOptions {omitNothingFields = True} ''RunnerStatus)
