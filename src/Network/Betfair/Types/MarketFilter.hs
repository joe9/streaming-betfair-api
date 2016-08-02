{-# OPTIONS_GHC -Wall    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.MarketFilter
  (MarketFilter(..))
  where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

import Network.Betfair.Types.BettingType (BettingType)

data MarketFilter =
  MarketFilter {countryCodes      :: Maybe [String]
               ,bettingTypes      :: [BettingType]
               ,turnInPlayEnabled :: Maybe Bool
               ,marketTypes       :: Maybe [String]
               ,venues            :: Maybe [String]
               ,marketIds         :: Maybe [String]
               ,eventTypeIds      :: Maybe [String]
               ,eventIds          :: Maybe [String]
               ,bspMarket         :: Maybe Bool}
  deriving (Eq,Show,Read)

-- this is what deriveDefault does anyway
-- instance Default MarketSort where def = FIRST_TO_START

-- $(deriveJSON id ''Record)
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''MarketFilter)

deriveDefault ''MarketFilter
