{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.MarketDataFilter
  (MarketDataFilter(..))
  where

import BasicPrelude
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Data.Default
-- import Data.Default.TH             (deriveDefault)
import Betfair.StreamingAPI.Types.Field

data MarketDataFilter =
  MarketDataFilter {ladderLevels :: Maybe Integer
                   ,fields :: [Field]}
  deriving (Eq,Show,Read)

-- this is what deriveDefault does anyway
-- instance Default MarketSort where def = FIRST_TO_START
-- $(deriveJSON id ''Record)
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''MarketDataFilter)

-- deriveDefault ''MarketDataFilter
instance Default MarketDataFilter where
  def =
    MarketDataFilter (Just 3)
                     [EX_BEST_OFFERS_DISP,EX_TRADED]
