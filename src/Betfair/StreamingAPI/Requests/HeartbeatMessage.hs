{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Requests.HeartbeatMessage
  (HeartbeatMessage(..))
  where

import BasicPrelude  hiding (id)
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Data.Default
--
import Betfair.StreamingAPI.API.AddId

data HeartbeatMessage =
  HeartbeatMessage {op :: Text
                   ,id :: Int}
  deriving (Eq,Read,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''HeartbeatMessage)

-- deriveDefault ''HeartbeatMessage
instance Default HeartbeatMessage where
  def = HeartbeatMessage "Heartbeat" 0

instance AddId HeartbeatMessage where
  addId o i = o {id = i}
