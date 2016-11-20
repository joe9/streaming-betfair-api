{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.Field
  ( Field(..)
  ) where

import Text.PrettyPrint.GenericPretty
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Protolude

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
  deriving (Eq, Show, Generic, Pretty, Read)

$(deriveJSON defaultOptions {omitNothingFields = True} ''Field)
