{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Requests.HeartbeatMessage
  ( HeartbeatMessage(..)
  , defaultHeartbeatMessage
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

--
import Betfair.StreamingAPI.API.AddId

data HeartbeatMessage = HeartbeatMessage
  { op :: Text
  , id :: Int
  } deriving (Eq, Read, Show, Generic, Pretty)

$(deriveJSON defaultOptions {omitNothingFields = True} ''HeartbeatMessage)

defaultHeartbeatMessage :: HeartbeatMessage
defaultHeartbeatMessage = HeartbeatMessage "Heartbeat" 0

instance AddId HeartbeatMessage where
  addId o i = o {id = i}
