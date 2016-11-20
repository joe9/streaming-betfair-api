{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.SegmentType
  ( SegmentType(..)
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

data SegmentType
  = SEG_START
  | SEG
  | SEG_END
  deriving (Eq, Show, Generic, Pretty, Read)

$(deriveJSON defaultOptions {omitNothingFields = True} ''SegmentType)
