{-# OPTIONS_GHC -Wall   #-}

module Network.Betfair.API.Request
  (request
  ,connect)
  where

import qualified Data.ByteString.Lazy as L

import Control.Monad.RWS
import Data.Aeson
import Data.Default
import Network.Connection
import Network.Socket hiding (connect)

import WriterLog

request :: ToJSON a
        => a -> RWST Connection Log s IO ()
request r =
  ask >>=
  (\c -> (groomedLog . L.toStrict . encode) r >>= lift . connectionPut c)

host :: String
-- for pre-production
host = "stream-api-integration.betfair.com"
-- for production
-- url = "stream-api.betfair.com"

port :: PortNumber
port = 443

connect :: IO Connection
connect =
  initConnectionContext >>=
  flip connectTo
       (ConnectionParams host
                         port
                         (Just def)
                         Nothing)
