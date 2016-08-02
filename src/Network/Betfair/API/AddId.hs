{-# OPTIONS_GHC -Wall    #-}

module Network.Betfair.API.AddId (AddId, addId) where

import Data.Aeson

class ToJSON a =>
      AddId a  where
  addId :: a -> Integer -> a
