{-# OPTIONS_GHC -Wall       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.RunnerChange
  (RunnerChange(..))
  where

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)

data RunnerChange =
  RunnerChange {tv    :: Maybe Double -- The total amount matched. This value is truncated at 2dp.
               ,
                -- (Level,Price,Volume)
                batb  :: [[Double]] -- Best Available To Back - LevelPriceVol triple delta of price changes, keyed by level (0 vol is remove)
               ,
                -- (Price,Volume)
                spb   :: [[Double]] -- Starting Price Back - PriceVol tuple delta of price changes (0 vol is remove)
               ,
                -- (Level,Price,Volume)
                bdatl :: [[Double]] -- Best Display Available To Lay (includes virtual prices)- LevelPriceVol triple delta of price changes, keyed by level (0 vol is remove)
               ,
                -- (Price,Volume)
                trd   :: [[Double]] -- Traded - PriceVol tuple delta of price changes (0 vol is remove)
               ,spf   :: Maybe Double -- Starting Price Far - The far starting price (or null if un-changed)
               ,ltp   :: Maybe Double -- Last Traded Price - The last traded price (or null if un-changed)
               ,
                -- (Price,Volume)
                atb   :: [[Double]] -- Available To Back - PriceVol tuple delta of price changes (0 vol is remove)
               ,
                -- (Price,Volume)
                spl   :: [[Double]] -- Starting Price Lay - PriceVol tuple delta of price changes (0 vol is remove)
               ,spn   :: Double -- Starting Price Near - The far starting price (or null if un-changed)
               ,
                -- (Price,Volume)
                atl   :: Maybe Bool -- Available To Lay - PriceVol tuple delta of price changes (0 vol is remove)
               ,
                -- (Level,Price,Volume)
                batl  :: Maybe Bool -- Best Available To Lay - LevelPriceVol triple delta of price changes, keyed by level (0 vol is remove)
               ,id    :: Integer  -- Selection Id - the id of the runner (selection)
               ,hc    :: Maybe Integer  -- Handicap - the handicap of the runner (selection) (null if not applicable)
               ,
                -- (Level,Price,Volume)
                bdatb :: Integer  -- Best Display Available To Back (includes virtual prices)- LevelPriceVol triple delta of price changes, keyed by level (0 vol is remove)
               }
  deriving (Eq,Show)

-- deriveDefault ''RunnerChange
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''RunnerChange)
