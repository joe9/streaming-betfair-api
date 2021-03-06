{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.RunnerDefinition
  ( RunnerDefinition(..)
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

--
import Betfair.StreamingAPI.Types.RunnerStatus (RunnerStatus)

type DateString = Text

data RunnerDefinition = RunnerDefinition
  { sortPriority     :: Integer
  , removalDate      :: Maybe DateString
  , id               :: Integer -- Selection Id - the id of the runner (selection)
  , hc               :: Maybe Double -- Handicap - the handicap of the runner (selection) (null if not applicable)
  , adjustmentFactor :: Maybe Double
  , bsp              :: Maybe Double
  , status           :: RunnerStatus
  } deriving (Eq, Read, Show, Generic, Pretty)

$(deriveJSON defaultOptions {omitNothingFields = True} ''RunnerDefinition)
