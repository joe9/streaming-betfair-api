{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.AddId
  ( AddId
  , addId
  ) where

import Data.Aeson
import Protolude

class ToJSON a =>
      AddId a where
  addId :: a -> Int -> a
