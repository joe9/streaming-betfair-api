{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.MarketDataFilter
  (MarketDataFilter(..))
  where

import Data.Text
import Data.Aeson.TH               (Options (omitNothingFields),
                                    defaultOptions, deriveJSON)
import Data.Default
import Data.Default.TH             (deriveDefault)
import Network.Betfair.Types.Field

data MarketDataFilter =
  MarketDataFilter {ladderLevels :: Maybe Integer
                   ,fields       :: [Field]}
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
