{-# OPTIONS_GHC -Wall       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.Betfair.Requests.AuthenticationMessage
  (authentication, AuthenticationMessage(..))
  where

import Control.Monad.RWS
import Network.Connection
import Data.Default
import Prelude hiding (id)
import Data.Aeson.TH
       (Options(omitNothingFields), defaultOptions, deriveJSON)
import Data.Default.TH (deriveDefault)

import Network.Betfair.API.Request

import WriterLog

data AuthenticationMessage =
  AuthenticationMessage {op      :: String
                        ,id      :: Integer -- Client generated unique id to link request with response (like json rpc)
                        ,session :: String
                        ,appKey  :: String}
  deriving (Eq,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''AuthenticationMessage)

deriveDefault ''AuthenticationMessage
-- instance Default HeartbeatMessage where
--   def = HeartbeatMessage "Heartbeat" def

authentication :: Integer -> String -> String -> RWST Connection Log s IO ()
authentication i s = request . AuthenticationMessage "Authentication" i s
