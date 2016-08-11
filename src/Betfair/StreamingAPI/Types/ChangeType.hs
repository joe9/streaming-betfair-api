{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.ChangeType
  (ChangeType(..))
  where
import BasicPrelude

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

-- Change Type - set to indicate the type of change - if null this is a delta)
data ChangeType
  = SUB_IMAGE
  | RESUB_DELTA
  | HEARTBEAT
  deriving (Eq,Show,Read)

deriveDefault ''ChangeType

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''ChangeType)
