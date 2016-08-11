{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Betfair.StreamingAPI.Types.OrderMarketChange
  (OrderMarketChange(..))
  where

import BasicPrelude
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
--
import Betfair.StreamingAPI.Types.OrderRunnerChange (OrderRunnerChange)

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
