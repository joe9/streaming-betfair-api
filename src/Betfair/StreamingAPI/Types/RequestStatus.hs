{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.RequestStatus
  ( RequestStatus(..)
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

data RequestStatus
  = SUCCESS
  | FAILURE
  deriving (Eq, Show, Generic, Pretty, Read)

$(deriveJSON defaultOptions {omitNothingFields = True} ''RequestStatus)
