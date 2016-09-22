{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.Side
  ( Side(..)
  ) where

import BasicPrelude
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

-- Side - the side of the order B - Back, L - Lay
data Side
  = B
  | L
  deriving (Eq, Read, Show)

deriveDefault ''Side

$(deriveJSON defaultOptions {omitNothingFields = True} ''Side)
