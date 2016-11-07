{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.SegmentType
  ( SegmentType(..)
  ) where

import Protolude
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)

data SegmentType
  = SEG_START
  | SEG
  | SEG_END
  deriving (Eq, Show, Read)

$(deriveJSON defaultOptions {omitNothingFields = True} ''SegmentType)
