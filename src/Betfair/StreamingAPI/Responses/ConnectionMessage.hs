{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Betfair.StreamingAPI.Responses.ConnectionMessage
  ( ConnectionMessage(..)
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

data ConnectionMessage = ConnectionMessage
  { op           :: Text
  , connectionId :: Text -- The connection id
  } deriving (Eq, Read, Show, Generic, Pretty)

$(deriveJSON defaultOptions {omitNothingFields = True} ''ConnectionMessage)
