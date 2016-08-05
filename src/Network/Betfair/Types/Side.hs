{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.Side
  (Side(..))
  where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)
import Data.Text

-- Side - the side of the order B - Back, L - Lay
data Side
  = B
  | L
  deriving (Eq,Read,Show)

deriveDefault ''Side

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''Side)
