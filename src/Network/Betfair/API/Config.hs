{-# OPTIONS_GHC -Wall     #-}

module Network.Betfair.API.Config
  (Config(..)
  ,AppKey)
  where

import Data.Default

import Network.Betfair.API.CommonTypes

data Config =
  Config {username      :: Text
         ,password      :: Text
         ,appKey        :: AppKey
         ,delayedAppKey :: AppKey}
  deriving (Eq,Read,Show)

instance Default Config where
  def = Config "" "" "" ""
