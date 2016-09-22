{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.SegmentType
  ( SegmentType(..)
  ) where

import BasicPrelude
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

data SegmentType
  = SEG_START
  | SEG
  | SEG_END
  deriving (Eq, Show, Read)

deriveDefault ''SegmentType

$(deriveJSON defaultOptions {omitNothingFields = True} ''SegmentType)
