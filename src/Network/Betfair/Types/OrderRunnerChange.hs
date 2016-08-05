{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.OrderRunnerChange
  (OrderRunnerChange(..))
  where

import Data.Aeson.TH               (Options (omitNothingFields),
                                    defaultOptions, deriveJSON)
import Network.Betfair.Types.Order (Order)

data OrderRunnerChange =
  OrderRunnerChange {mb        :: [[Double]] -- Matched Backs - matched amounts by distinct matched price on the Back side for this runner (selection)
                    ,uo        :: [Order] -- Unmatched Orders - orders on this runner (selection) that are not fully matched
                    ,id        :: Integer -- Selection Id - the id of the runner (selection)
                    ,hc        :: Maybe Double -- Handicap - the handicap of the runner (selection) (null if not applicable)
                    ,fullImage :: Maybe Bool
                    ,ml        :: [[Double]] -- Matched Lays - matched amounts by distinct matched price on the Lay side for this runner (selection)
                    }
  deriving (Eq,Read,Show)

-- deriveDefault ''OrderRunnerChange
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''OrderRunnerChange)
