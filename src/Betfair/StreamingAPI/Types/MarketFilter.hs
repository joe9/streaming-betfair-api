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
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Data.Default
--
import Betfair.StreamingAPI.Types.BettingType (BettingType (ODDS))

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

--- event types
--- 1 Soccer 13904
--- 2 Tennis 3615
--- 3 Golf 89
--- 4 Cricket 170
--- 5 Rugby Union 10
--- 6 Boxing 51
--- 7 Horse Racing 599
--- 8 Motor Sport 11
--- 10 Special Bets 16
--- 11 Cycling 17
--- 1477 Rugby League 28
--- 3503 Darts 2
--- 3988 Athletics 126
--- 4339 Greyhound Racing 181
--- 6231 Financial Bets 3
--- 6422 Snooker 3
--- 6423 American Football 32
--- 7511 Baseball 68
--- 7522 Basketball 143
--- 7523 Hockey 12
--- 7524 Ice Hockey 1
--- 136332 Chess 15
--- 315220 Poker 1
--- 468328 Handball 48
--- 620576 Swimming 4
--- 627555 Badminton 10
--- 998917 Volleyball 26
--- 2152880 Gaelic Games 24
--- 2378961 Politics 43
--- 2593174 Table Tennis 4
--- 2872194 Beach Volleyball 6
--- 2901849 Water Polo 16
--- 26420387 Mixed Martial Arts 77
--- 27589895 Olympics 2016 156
-- deriveDefault ''MarketFilter
instance Default MarketFilter where
  def =
    MarketFilter Nothing
                 [ODDS]
                 (Just True)
                 (Just ["MATCH_ODDS"])
                 Nothing
                 Nothing
                 (Just ["2"])
                 Nothing
                 Nothing
