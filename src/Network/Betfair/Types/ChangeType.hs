{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Network.Betfair.Types.ChangeType
  (ChangeType(..))
  where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)
import Data.Text

-- Change Type - set to indicate the type of change - if null this is a delta)
data ChangeType
  = SUB_IMAGE
  | RESUB_DELTA
  | HEARTBEAT
  deriving (Eq,Show,Read)

deriveDefault ''ChangeType

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''ChangeType)
