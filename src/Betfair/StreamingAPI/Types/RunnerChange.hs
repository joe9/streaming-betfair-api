{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.RunnerChange
  ( RunnerChange(..)
  ) where

import Text.PrettyPrint.GenericPretty
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Protolude

data RunnerChange = RunnerChange
  { tv    :: Maybe Double -- The total amount matched. This value is truncated at 2dp.
    -- (Level,Price,Volume)
  , batb  :: Maybe [[Double]] -- Best Available To Back - LevelPriceVol triple delta of price changes, keyed by level (0 vol is remove)
    -- (Price,Volume)
  , spb   :: Maybe [[Double]] -- Starting Price Back - PriceVol tuple delta of price changes (0 vol is remove)
    -- (Level,Price,Volume)
  , bdatl :: Maybe [[Double]] -- Best Display Available To Lay (includes virtual prices)- LevelPriceVol triple delta of price changes, keyed by level (0 vol is remove)
    -- (Price,Volume)
  , trd   :: Maybe [[Double]] -- Traded - PriceVol tuple delta of price changes (0 vol is remove)
  , spf   :: Maybe Double -- Starting Price Far - The far starting price (or null if un-changed)
  , ltp   :: Maybe Double -- Last Traded Price - The last traded price (or null if un-changed)
    -- (Price,Volume)
  , atb   :: Maybe [[Double]] -- Available To Back - PriceVol tuple delta of price changes (0 vol is remove)
    -- (Price,Volume)
  , spl   :: Maybe [[Double]] -- Starting Price Lay - PriceVol tuple delta of price changes (0 vol is remove)
  , spn   :: Maybe Double -- Starting Price Near - The far starting price (or null if un-changed)
    -- (Price,Volume)
  , atl   :: Maybe [[Double]] -- Available To Lay - PriceVol tuple delta of price changes (0 vol is remove)
    -- (Level,Price,Volume)
  , batl  :: Maybe [[Double]] -- Best Available To Lay - LevelPriceVol triple delta of price changes, keyed by level (0 vol is remove)
  , id    :: Integer -- Selection Id - the id of the runner (selection)
  , hc    :: Maybe Double -- Handicap - the handicap of the runner (selection) (null if not applicable)
    -- (Level,Price,Volume)
  , bdatb :: Maybe [[Double]] -- Best Display Available To Back (includes virtual prices)- LevelPriceVol triple delta of price changes, keyed by level (0 vol is remove)
  } deriving (Eq, Read, Show, Generic, Pretty)

$(deriveJSON defaultOptions {omitNothingFields = True} ''RunnerChange)
