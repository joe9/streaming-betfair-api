{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.AddId
  (AddId
  ,addId
  ,timeInMicroseconds)
  where

import BasicPrelude
import Data.Aeson
import Data.Ratio
import Data.Time.Clock.POSIX

class ToJSON a =>
      AddId a  where
  addId :: a -> Int -> a

-- to be used as Id
timeInMicroseconds :: IO Int
timeInMicroseconds = fromIntegral . numerator . toRational . (* 1000000) <$> getPOSIXTime
