{-# OPTIONS_GHC -Wall   #-}

module Network.Betfair.API.Request
  (request)
  where

import qualified Data.ByteString.Lazy as L

import Control.Monad.RWS
import Data.Aeson
import Data.Default
import Network.Connection

import WriterLog

request :: ToJSON a
        => a -> RWST Connection Log s IO ()
request r =
  ask >>=
  (\c -> (groomedLog . L.toStrict . encode) r >>= lift . connectionPut c)
