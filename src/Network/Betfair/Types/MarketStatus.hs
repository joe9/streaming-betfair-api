{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.MarketStatus
  (MarketStatus(..))
  where

import Data.Text
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

data MarketStatus
  = INACTIVE
  | OPEN
  | SUSPENDED
  | CLOSED
  deriving (Eq,Show,Read)

deriveDefault ''MarketStatus

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''MarketStatus)
