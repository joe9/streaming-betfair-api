{-# OPTIONS_GHC -Wall    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.RunnerDefinition
  (RunnerDefinition(..))
  where

import Data.Aeson.TH                      (Options (omitNothingFields),
                                           defaultOptions, deriveJSON)
import Data.Default.TH                    (deriveDefault)
import Network.Betfair.Types.RunnerStatus (RunnerStatus)

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
