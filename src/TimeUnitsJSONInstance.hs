{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TimeUnitsJSONInstance
  where

import Protolude hiding (FilePath)
import Data.Time.Units
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
--
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''Microsecond)
