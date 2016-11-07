{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.ChangeType
  ( ChangeType(..)
  ) where

import Protolude
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)

-- Change Type - set to indicate the type of change - if null this is a delta)
data ChangeType
  = SUB_IMAGE
  | RESUB_DELTA
  | HEARTBEAT
  deriving (Eq, Show, Read)


$(deriveJSON defaultOptions {omitNothingFields = True} ''ChangeType)
