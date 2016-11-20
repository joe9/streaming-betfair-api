{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.AddId
  ( AddId
  , addId
  ) where

import Text.PrettyPrint.GenericPretty
import Data.Aeson
import Protolude

class ToJSON a =>
      AddId a where
  addId :: a -> Int -> a
