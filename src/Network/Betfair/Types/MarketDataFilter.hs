{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.MarketDataFilter
  (MarketDataFilter(..))
  where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

import Network.Betfair.Types.Field (Field)

data MarketDataFilter =
  MarketDataFilter {ladderLevels :: Maybe Integer
                   ,fields       :: [Field]}
  deriving (Eq,Show,Read)

-- this is what deriveDefault does anyway
-- instance Default MarketSort where def = FIRST_TO_START

-- $(deriveJSON id ''Record)
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''MarketDataFilter)

deriveDefault ''MarketDataFilter
