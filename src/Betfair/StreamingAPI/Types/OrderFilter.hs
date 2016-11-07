{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.OrderFilter
  ( OrderFilter(..)
  ) where

import Protolude
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)

data OrderFilter = OrderFilter
  { accountIds :: [Integer]
  } deriving (Eq, Read, Show)

-- instance Default MarketSort where def = FIRST_TO_START
-- $(deriveJSON id ''Record)
$(deriveJSON defaultOptions {omitNothingFields = True} ''OrderFilter)

