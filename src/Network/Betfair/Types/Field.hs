{-# OPTIONS_GHC -Wall    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.Field
  (Field(..))
  where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

data Field
  = EX_BEST_OFFERS_DISP
  | EX_BEST_OFFERS
  | EX_ALL_OFFERS
  | EX_TRADED
  | EX_TRADED_VOL
  | EX_LTP
  | EX_MARKET_DEF
  | SP_TRADED
  | SP_PROJECTED
  deriving (Eq,Show,Read)

deriveDefault ''Field

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''Field)
