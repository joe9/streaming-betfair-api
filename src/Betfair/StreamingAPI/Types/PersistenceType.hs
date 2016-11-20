{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.PersistenceType
  ( PersistenceType(..)
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

-- Persistence Type - whether the order will persist at in play or not (L = LAPSE, P = PERSIST, MOC = Market On Close)
data PersistenceType
  = L
  | P
  | MOC
  deriving (Eq, Read, Show, Generic, Pretty)

$(deriveJSON defaultOptions {omitNothingFields = True} ''PersistenceType)
