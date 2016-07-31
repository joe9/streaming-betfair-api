{-# OPTIONS_GHC -Wall        #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Network.Betfair.Types.SegmentType
  (SegmentType(..))
  where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

data SegmentType
  = SEG_START
  | SEG
  | SEG_END
  deriving (Eq,Show,Read)

deriveDefault ''SegmentType
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''SegmentType)
