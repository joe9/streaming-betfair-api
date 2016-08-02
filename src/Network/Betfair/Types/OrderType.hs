{-# OPTIONS_GHC -Wall  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.OrderType
  (OrderType(..))
  where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

-- Order Type - the type of the order (L = LIMIT, MOC = MARKET_ON_CLOSE, LOC = LIMIT_ON_CLOSE)
data OrderType
  = L
  | LOC
  | MOC
  deriving (Eq,Read,Show)

deriveDefault ''OrderType
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''OrderType)
