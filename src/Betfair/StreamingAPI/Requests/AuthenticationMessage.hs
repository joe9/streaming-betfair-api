{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Requests.AuthenticationMessage
  ( AuthenticationMessage(..)
  , defaultAuthenticationMessage
  ) where

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Protolude     hiding (id)

import Betfair.StreamingAPI.API.AddId

data AuthenticationMessage = AuthenticationMessage
  { op      :: Text
  , id      :: Int -- Client generated unique id to link request with response (like json rpc)
  , appKey  :: Text
  , session :: Text
  } deriving (Eq, Read, Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''AuthenticationMessage)

defaultAuthenticationMessage :: AuthenticationMessage
defaultAuthenticationMessage =
  AuthenticationMessage "authentication" 0 undefined undefined

instance AddId AuthenticationMessage where
  addId o i = o {id = i}
