{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Requests.AuthenticationMessage
  (AuthenticationMessage(..))
  where
import BasicPrelude

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Data.Default
import Data.Text
import Prelude       hiding (id)
-- import Data.Default.TH (deriveDefault)
import Betfair.StreamingAPI.API.AddId

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
