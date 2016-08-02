{-# OPTIONS_GHC -Wall  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.Betfair.Requests.HeartbeatMessage
  (HeartbeatMessage(..))
  where

import Data.Aeson.TH
       (Options(omitNothingFields), defaultOptions, deriveJSON)
-- import Data.Default.TH (deriveDefault)
import Data.Default
import Prelude hiding (id)

data HeartbeatMessage =
  HeartbeatMessage {op :: String
                   ,id :: Integer}
  deriving (Eq,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''HeartbeatMessage)

-- deriveDefault ''HeartbeatMessage
instance Default HeartbeatMessage where
  def = HeartbeatMessage "Heartbeat" def
