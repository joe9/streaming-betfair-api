{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.Side
   ( Side(..)
   ) where

import           Data.Aeson.TH   (Options (omitNothingFields),
                                  defaultOptions, deriveJSON)
import           Data.Default.TH (deriveDefault)

-- Side - the side of the order B - Back, L - Lay
data Side = B | L
   deriving (Eq, Show)

deriveDefault ''Side
$(deriveJSON defaultOptions {omitNothingFields = True} ''Side)
