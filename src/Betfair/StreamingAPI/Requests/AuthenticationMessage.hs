{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Requests.AuthenticationMessage
  ( AuthenticationMessage(..)
  ) where

import BasicPrelude  hiding (id)
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Data.Default

-- import Data.Default.TH (deriveDefault)
--
import Betfair.StreamingAPI.API.AddId

data AuthenticationMessage = AuthenticationMessage
  { op      :: Text
  , id      :: Int -- Client generated unique id to link request with response (like json rpc)
  , appKey  :: Text
  , session :: Text
  } deriving (Eq, Read, Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''AuthenticationMessage)

-- deriveDefault ''AuthenticationMessage
instance Default AuthenticationMessage where
  def = AuthenticationMessage "authentication" 0 undefined undefined

instance AddId AuthenticationMessage where
  addId o i = o {id = i}
