{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.MarketDataFilter
  ( MarketDataFilter(..)
  , defaultMarketDataFilter
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

import Betfair.StreamingAPI.Types.Field

data MarketDataFilter = MarketDataFilter
  { ladderLevels :: Maybe Integer
  , fields       :: [Field]
  } deriving (Eq, Show, Generic, Pretty, Read)

-- instance Default MarketSort where def = FIRST_TO_START
-- $(deriveJSON id ''Record)
$(deriveJSON defaultOptions {omitNothingFields = True} ''MarketDataFilter)

defaultMarketDataFilter :: MarketDataFilter
defaultMarketDataFilter =
  MarketDataFilter
    (Just 3)
    [EX_BEST_OFFERS_DISP, EX_TRADED, EX_TRADED_VOL, EX_LTP, EX_MARKET_DEF]
