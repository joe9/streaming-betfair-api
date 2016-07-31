{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.MarketChange
  (MarketChange(..))
  where

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)

import Network.Betfair.Types.MarketBettingType (MarketBettingType)
import Network.Betfair.Types.MarketStatus      (MarketStatus)

type DateString = String

data MarketChange =
  MarketChange {rc               :: Maybe [RunnerChange] -- Runner Changes - a list of changes to runners (or null if un-changed)
               ,img              :: Maybe Bool -- Image - replace existing prices / data with the data supplied: it is not a delta (or null if delta)
               ,tv               :: Maybe Double -- The total amount matched across the market. This value is truncated at 2dp (or null if un-changed)
               ,con              :: Maybe Bool -- Conflated - have more than a single change been combined (or null if not conflated)
               ,marketDefinition :: Maybe MarketDefinition -- Market Definition - the definition of the market (or null if un-changed)
               ,id               :: String -- Market Id - the id of the market
               }
  deriving (Eq,Show)

-- deriveDefault ''MarketChange
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''MarketChange)
