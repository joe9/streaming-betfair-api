{-# OPTIONS_GHC -Wall        #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.OrderFilter
  (OrderFilter(..))
  where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

data OrderFilter =
  OrderFilter {accountIds :: [Integer]}
  deriving (Eq,Read,Show)

-- this is what deriveDefault does anyway
-- instance Default MarketSort where def = FIRST_TO_START
-- $(deriveJSON id ''Record)
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''OrderFilter)

deriveDefault ''OrderFilter
