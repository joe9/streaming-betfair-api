{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.MarketStatus
  ( MarketStatus(..)
  ) where

import           Data.Aeson.TH                  (Options (omitNothingFields),
                                                 defaultOptions,
                                                 deriveJSON)
import           Protolude
import           Text.PrettyPrint.GenericPretty
-- import qualified Text.PrettyPrint.Leijen.Text   as PP

data MarketStatus
  = INACTIVE
  | OPEN
  | SUSPENDED
  | CLOSED
  deriving (Eq, Show, Generic, Pretty, Read, Enum)

$(deriveJSON defaultOptions {omitNothingFields = True} ''MarketStatus)

-- instance Pretty MarketStatus where
--   pretty INACTIVE  = PP.text "Inactive"
--   pretty OPEN      = PP.text "Open"
--   pretty SUSPENDED = PP.text "Suspended"
--   pretty CLOSED    = PP.text "Closed"
