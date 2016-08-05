{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Network.Betfair.Types.RunnerStatus
  (RunnerStatus(..))
  where

import Data.Text
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

data RunnerStatus
  = ACTIVE
  | WINNER
  | LOSER
  | REMOVED_VACANT
  | REMOVED
  | HIDDEN
  deriving (Eq,Show,Read)

deriveDefault ''RunnerStatus

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''RunnerStatus)
