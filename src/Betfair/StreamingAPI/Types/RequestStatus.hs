{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.RequestStatus
  ( RequestStatus(..)
  ) where

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Protolude

data RequestStatus
  = SUCCESS
  | FAILURE
  deriving (Eq, Show, Read)

$(deriveJSON defaultOptions {omitNothingFields = True} ''RequestStatus)
