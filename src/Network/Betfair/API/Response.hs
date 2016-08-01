{-# OPTIONS_GHC -Wall        #-}

module Network.Betfair.API.Response
  (response)
  where

import qualified Data.ByteString.Lazy as L

import Control.Monad.RWS
import Data.Aeson
import Data.Maybe
import Network.Connection
import Safe

import Network.Betfair.Responses.ConnectionMessage
import Network.Betfair.Responses.MarketChangeMessage
import Network.Betfair.Responses.OrderChangeMessage
import Network.Betfair.Responses.StatusMessage

import WriterLog

data Response
  = Connection ConnectionMessage
  | MarketChange MarketChangeMessage
  | OrderChange OrderChangeMessage
  | Status StatusMessage

response :: RWST Connection Log s IO Response
response =
  ask >>= lift . connectionGetLine 16384 >>= groomedLog >>=
  lift . return . parseResponse . L.fromStrict

parseResponse :: L.ByteString -> Response
parseResponse b
  | isJust (decode b :: Maybe ConnectionMessage) =
    (Connection .
     fromJustNote "response: could not parse heartbeat" .
     (decode :: L.ByteString -> Maybe ConnectionMessage)) b
  | isJust (decode b :: Maybe MarketChangeMessage) =
    (MarketChange .
     fromJustNote "response: could not parse heartbeat" .
     (decode :: L.ByteString -> Maybe MarketChangeMessage)) b
  | isJust (decode b :: Maybe OrderChangeMessage) =
    (OrderChange .
     fromJustNote "response: could not parse heartbeat" .
     (decode :: L.ByteString -> Maybe OrderChangeMessage)) b
  | isJust (decode b :: Maybe StatusMessage) =
    (Status .
     fromJustNote "response: could not parse heartbeat" .
     (decode :: L.ByteString -> Maybe StatusMessage)) b
  | otherwise = error $ "response: could not parse bytestring" ++ show b
