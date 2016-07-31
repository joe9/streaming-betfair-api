{-# OPTIONS_GHC -Wall        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.OrderMarketChange
  (OrderMarketChange(..))
  where

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)

import Network.Betfair.Types.OrderRunnerChange (OrderRunnerChange)

type DateString = String

data OrderMarketChange =
  OrderMarketChange {accountId :: Integer
                    ,orc       :: [OrderRunnerChange] -- Order Changes - a list of changes to orders on a selection
                    ,closed    :: Bool
                    ,id        :: String -- Market Id - the id of the market the order is on
                    }
  deriving (Eq,Show)

-- deriveDefault ''OrderMarketChange
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''OrderMarketChange)
