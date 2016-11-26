{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass   #-}

module TimeUnitsJSONInstance where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Time.Units
import GHC.Generics
import Protolude       hiding (FilePath)

import Text.PrettyPrint.GenericPretty

$(deriveJSON defaultOptions {omitNothingFields = True} ''Microsecond)

deriving instance Generic Microsecond
deriving instance Pretty Microsecond

