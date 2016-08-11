{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Config
  (Config(..)
  ,AppKey)
  where
import BasicPrelude

import Data.Default
import Data.Text
import Betfair.StreamingAPI.API.CommonTypes

data Config =
  Config {username      :: Text
         ,password      :: Text
         ,appKey        :: AppKey
         ,delayedAppKey :: AppKey}
  deriving (Eq,Read,Show)

instance Default Config where
  def = Config "" "" "" ""
