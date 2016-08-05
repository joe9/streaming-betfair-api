{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.MarketFilter
  (MarketFilter(..))
  where

import Data.Text
import Data.Aeson.TH                     (Options (omitNothingFields),
                                          defaultOptions, deriveJSON)
import Data.Default.TH                   (deriveDefault)
import Network.Betfair.Types.BettingType (BettingType)

data MarketFilter =
  MarketFilter {countryCodes      :: Maybe [Text]
               ,bettingTypes      :: [BettingType]
               ,turnInPlayEnabled :: Maybe Bool
               ,marketTypes       :: Maybe [Text]
               ,venues            :: Maybe [Text]
               ,marketIds         :: Maybe [Text]
               ,eventTypeIds      :: Maybe [Text]
               ,eventIds          :: Maybe [Text]
               ,bspMarket         :: Maybe Bool}
  deriving (Eq,Show,Read)

-- this is what deriveDefault does anyway
-- instance Default MarketSort where def = FIRST_TO_START
-- $(deriveJSON id ''Record)
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''MarketFilter)

deriveDefault ''MarketFilter
