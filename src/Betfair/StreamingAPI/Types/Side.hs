{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.Side
  ( Side(..)
  ) where

import Protolude
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)

-- Side - the side of the order B - Back, L - Lay
data Side
  = B
  | L
  deriving (Eq, Read, Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''Side)
