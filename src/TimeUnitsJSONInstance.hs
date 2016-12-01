{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module TimeUnitsJSONInstance where

import Data.Aeson.TH         (Options (omitNothingFields),
                              defaultOptions, deriveJSON)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Units
import GHC.Generics
import Protolude             hiding (FilePath)

import Text.PrettyPrint.GenericPretty

TODO NOT used anymore, delete it

$(deriveJSON defaultOptions {omitNothingFields = True} ''Microsecond)

-- deriving instance Generic Microsecond
-- deriving instance Pretty Microsecond
-- instance Pretty Microsecond where
--   pretty ( Microsecond m) =
-- to use for the ssLastMarketSubscriptionMessageSentAt
timeInMicroseconds :: IO Microsecond
timeInMicroseconds =
  fromMicroseconds . fromIntegral . numerator . toRational . (* 1000000) <$>
  getPOSIXTime
