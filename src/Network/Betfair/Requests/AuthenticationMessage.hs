{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Requests.AuthenticationMessage
  (AuthenticationMessage(..))
  where

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Data.Default
import Prelude       hiding (id)
-- import Data.Default.TH (deriveDefault)
import Network.Betfair.API.AddId

data AuthenticationMessage =
  AuthenticationMessage {op      :: Text
                        ,id      :: Integer -- Client generated unique id to link request with response (like json rpc)
                        ,appKey  :: Text
                        ,session :: Text}
  deriving (Eq,Read,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''AuthenticationMessage)

-- deriveDefault ''AuthenticationMessage
instance Default AuthenticationMessage where
  def = AuthenticationMessage "authentication" def undefined undefined

instance AddId AuthenticationMessage where
  addId o i = o {id = i}
