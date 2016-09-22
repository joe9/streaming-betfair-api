{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Betfair.StreamingAPI.Responses.ConnectionMessage
  ( ConnectionMessage(..)
  ) where

import BasicPrelude
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)

data ConnectionMessage = ConnectionMessage
  { op           :: Text
  , connectionId :: Text -- The connection id
  } deriving (Eq, Read, Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''ConnectionMessage)
