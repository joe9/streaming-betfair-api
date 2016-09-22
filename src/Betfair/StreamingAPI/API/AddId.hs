{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.AddId
  ( AddId
  , addId
  ) where

import BasicPrelude
import Data.Aeson

class ToJSON a =>
      AddId a where
  addId :: a -> Int -> a
