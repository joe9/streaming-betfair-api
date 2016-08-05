{-# LANGUAGE OverloadedStrings #-}

module Network.Betfair.API.AddId
  (AddId
  ,addId)
  where

import Data.Aeson
import Data.Text

class ToJSON a =>
      AddId a  where
  addId :: a -> Integer -> a
