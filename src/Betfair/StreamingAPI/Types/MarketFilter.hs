{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.MarketFilter
  (MarketFilter(..))
  where

import BasicPrelude
import Betfair.StreamingAPI.Types.BettingType (BettingType)
import Data.Aeson.TH                          (Options (omitNothingFields),
                                               defaultOptions,
                                               deriveJSON)
import Data.Default.TH                        (deriveDefault)
import Data.Text

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
