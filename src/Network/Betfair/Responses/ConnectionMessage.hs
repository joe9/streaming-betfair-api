{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Betfair.Responses.ConnectionMessage
  (ConnectionMessage(..))
  where

import Data.Aeson.TH
       (Options(omitNothingFields), defaultOptions, deriveJSON)

data ConnectionMessage =
  ConnectionMessage {op           :: String
                    ,id           :: Integer -- Client generated unique id to link request with response (like json rpc)
                    ,connectionId :: String -- The connection id
                    }
  deriving (Eq,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''ConnectionMessage)
