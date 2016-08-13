{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Config
  (Config(..)
  ,AppKey)
  where

import BasicPrelude
import Betfair.StreamingAPI.API.CommonTypes
import Data.Default

data Config =
  Config {username      :: Text
         ,password      :: Text
         ,appKey        :: AppKey
         ,delayedAppKey :: AppKey}
  deriving (Eq,Read,Show)

instance Default Config where
  def = Config "" "" "" ""
