{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.OrderStatus
  ( OrderStatus(..)
  ) where

import Protolude
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)

-- Status - the status of the order (E = EXECUTABLE, EC = EXECUTION_COMPLETE)
data OrderStatus
  = E
  | EC
  deriving (Eq, Show, Read)


$(deriveJSON defaultOptions {omitNothingFields = True} ''OrderStatus)
