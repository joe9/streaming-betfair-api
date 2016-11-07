{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.OrderType
  ( OrderType(..)
  ) where

import Protolude
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)

-- Order Type - the type of the order (L = LIMIT, MOC = MARKET_ON_CLOSE, LOC = LIMIT_ON_CLOSE)
data OrderType
  = L
  | LOC
  | MOC
  deriving (Eq, Read, Show)


$(deriveJSON defaultOptions {omitNothingFields = True} ''OrderType)
