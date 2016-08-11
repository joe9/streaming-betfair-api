{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.PersistenceType
  (PersistenceType(..))
  where

import BasicPrelude
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

-- Persistence Type - whether the order will persist at in play or not (L = LAPSE, P = PERSIST, MOC = Market On Close)
data PersistenceType
  = L
  | P
  | MOC
  deriving (Eq,Read,Show)

deriveDefault ''PersistenceType

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''PersistenceType)
