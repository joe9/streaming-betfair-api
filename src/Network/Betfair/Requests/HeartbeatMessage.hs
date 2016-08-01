{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Requests.HeartbeatMessage (heartbeat) where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

import Network.Betfair.API.Request

data HeartbeatMessage = HeartbeatMessage
   { op :: String
   , id      :: Integer
   } deriving (Eq, Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''HeartbeatMessage)

deriveDefault ''HeartbeatMessage

heartbeat :: Integer -> RWST r w s m ()
heartbeat = request . HeartbeatMessage "Heartbeat"
