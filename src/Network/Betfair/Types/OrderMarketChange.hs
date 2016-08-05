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

data OrderMarketChange =
  OrderMarketChange {accountId :: Integer
                    ,orc       :: [OrderRunnerChange] -- Order Changes - a list of changes to orders on a selection
                    ,closed    :: Bool
                    ,id        :: Text -- Market Id - the id of the market the order is on
                    }
  deriving (Eq,Read,Show)

-- deriveDefault ''OrderMarketChange
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''OrderMarketChange)
