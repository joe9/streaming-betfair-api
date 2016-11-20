{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.OrderFilter
  ( OrderFilter(..)
  ) where

import Text.PrettyPrint.GenericPretty
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Protolude

data OrderFilter = OrderFilter
  { accountIds :: [Integer]
  } deriving (Eq, Read, Show, Generic, Pretty)

-- instance Default MarketSort where def = FIRST_TO_START
-- $(deriveJSON id ''Record)
$(deriveJSON defaultOptions {omitNothingFields = True} ''OrderFilter)
