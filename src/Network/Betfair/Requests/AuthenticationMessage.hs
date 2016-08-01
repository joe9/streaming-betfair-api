{-# OPTIONS_GHC -Wall       #-}

module Network.Betfair.Requests.AuthenticationMessage
  (AuthenticationMessage(..))
  where

import Data.Aeson.TH
       (Options(omitNothingFields), defaultOptions, deriveJSON)
import Data.Default.TH (deriveDefault)

import Network.Betfair.API.Request


data AuthenticationMessage =
  AuthenticationMessage {op      :: String
                        ,id      :: Integer -- Client generated unique id to link request with response (like json rpc)
                        ,session :: String
                        ,appKey  :: String}
  deriving (Eq,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''AuthenticationMessage)

deriveDefault ''AuthenticationMessage

authentication :: Integer -> String -> String -> RWST r w s m ()
authentication i s = request . Authentication "Authentication" i s
