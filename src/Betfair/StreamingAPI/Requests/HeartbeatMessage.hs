{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Requests.HeartbeatMessage
  (HeartbeatMessage(..))
  where
import BasicPrelude

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Data.Text
-- import Data.Default.TH (deriveDefault)
import Data.Default
import Betfair.StreamingAPI.API.AddId
import Prelude                   hiding (id)

data HeartbeatMessage =
  HeartbeatMessage {op :: Text
                   ,id :: Integer}
  deriving (Eq,Read,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''HeartbeatMessage)

-- deriveDefault ''HeartbeatMessage
instance Default HeartbeatMessage where
  def = HeartbeatMessage "Heartbeat" def

instance AddId HeartbeatMessage where
  addId o i = o {id = i}