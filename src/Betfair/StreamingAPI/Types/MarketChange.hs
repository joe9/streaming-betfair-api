{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.MarketChange
  (MarketChange(..))
  where

import BasicPrelude
import Data.Aeson.TH                               (Options (omitNothingFields),
                                                    defaultOptions,
                                                    deriveJSON)
--
import Betfair.StreamingAPI.Types.MarketDefinition (MarketDefinition)
import Betfair.StreamingAPI.Types.RunnerChange     (RunnerChange)

data MarketChange =
  MarketChange {rc               :: Maybe [RunnerChange] -- Runner Changes - a list of changes to runners (or null if un-changed)
               ,img              :: Maybe Bool -- Image - replace existing prices / data with the data supplied: it is not a delta (or null if delta)
               ,tv               :: Maybe Double -- The total amount matched across the market. This value is truncated at 2dp (or null if un-changed)
               ,con              :: Maybe Bool -- Conflated - have more than a single change been combined (or null if not conflated)
               ,marketDefinition :: Maybe MarketDefinition -- Market Definition - the definition of the market (or null if un-changed)
               ,id               :: Text -- Market Id - the id of the market
               }
  deriving (Eq,Read,Show)

-- deriveDefault ''MarketChange
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''MarketChange)
