{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.OrderStatus
  ( OrderStatus(..)
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

-- Status - the status of the order (E = EXECUTABLE, EC = EXECUTION_COMPLETE)
data OrderStatus
  = E
  | EC
  deriving (Eq, Show, Generic, Pretty, Read)

$(deriveJSON defaultOptions {omitNothingFields = True} ''OrderStatus)
