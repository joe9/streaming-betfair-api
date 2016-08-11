{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.RunnerDefinition
  (RunnerDefinition(..))
  where
import BasicPrelude

import Data.Aeson.TH                      (Options (omitNothingFields),
                                           defaultOptions, deriveJSON)
import Data.Default.TH                    (deriveDefault)
import Data.Text
import Betfair.StreamingAPI.Types.RunnerStatus (RunnerStatus)

type DateString = Text

data RunnerDefinition =
  RunnerDefinition {sortPriority     :: Integer
                   ,removalDate      :: Maybe DateString
                   ,id               :: Integer -- Selection Id - the id of the runner (selection)
                   ,hc               :: Maybe Double -- Handicap - the handicap of the runner (selection) (null if not applicable)
                   ,adjustmentFactor :: Maybe Double
                   ,bsp              :: Maybe Double
                   ,status           :: RunnerStatus}
  deriving (Eq,Read,Show)

deriveDefault ''RunnerDefinition

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''RunnerDefinition)
