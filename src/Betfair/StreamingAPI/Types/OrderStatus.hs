{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.OrderStatus
  ( OrderStatus(..)
  ) where

import BasicPrelude
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

-- Status - the status of the order (E = EXECUTABLE, EC = EXECUTION_COMPLETE)
data OrderStatus
  = E
  | EC
  deriving (Eq, Show, Read)

deriveDefault ''OrderStatus

$(deriveJSON defaultOptions {omitNothingFields = True} ''OrderStatus)
