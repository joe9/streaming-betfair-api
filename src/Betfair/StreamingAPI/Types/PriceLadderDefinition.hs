{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.PriceLadderDefinition
  ( PriceLadderDefinition(..)
  ) where

import Data.Aeson.TH                  (Options (fieldLabelModifier, omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

import Betfair.APING.Types.PriceLadderType (PriceLadderType)

data PriceLadderDefinition = PriceLadderDefinition
  { pltype :: PriceLadderType
  } deriving (Eq, Read, Show, Generic, Pretty)

$(deriveJSON
    defaultOptions {omitNothingFields = True, fieldLabelModifier = drop 2}
    ''PriceLadderDefinition)
