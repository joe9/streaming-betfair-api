{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module TimeUnitsJSONInstance where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Time.Units
import Protolude       hiding (FilePath)

--
$(deriveJSON defaultOptions {omitNothingFields = True} ''Microsecond)
