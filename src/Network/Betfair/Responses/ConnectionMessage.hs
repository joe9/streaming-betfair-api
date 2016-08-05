{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Betfair.Responses.ConnectionMessage
  (ConnectionMessage(..))
  where

import Data.Aeson.TH
       (Options(omitNothingFields), defaultOptions, deriveJSON)

data ConnectionMessage =
  ConnectionMessage {op           :: Text
                    ,connectionId :: Text -- The connection id
                    }
  deriving (Eq,Read,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''ConnectionMessage)
