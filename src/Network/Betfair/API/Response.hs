{-# OPTIONS_GHC -Wall        #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Betfair.API.Response
  (response)
  where

import qualified Data.ByteString.Lazy as L

import Control.Monad.RWS
import Data.Aeson
import Data.Maybe
import Network.Connection
import Safe

import qualified Network.Betfair.Responses.ConnectionMessage as C
import qualified Network.Betfair.Responses.MarketChangeMessage as M
import qualified Network.Betfair.Responses.OrderChangeMessage as O
import qualified Network.Betfair.Responses.StatusMessage as S

import WriterLog

data Response
  = Connection C.ConnectionMessage
  | MarketChange M.MarketChangeMessage
  | OrderChange O.OrderChangeMessage
  | Status S.StatusMessage

response :: RWST Connection Log s IO Response
response =
  ask >>= lift . connectionGetLine 16384 >>= groomedLog >>=
  lift . return . parseResponse . L.fromStrict

parseResponse :: L.ByteString -> Response
parseResponse b
  | isJust (decode b :: Maybe C.ConnectionMessage) && cop == "connection" =
    (Connection .
     fromJustNote "response: could not parse connection" .
     (decode :: L.ByteString -> Maybe C.ConnectionMessage)) b
  | isJust (decode b :: Maybe M.MarketChangeMessage) && mop == "mcm" =
    (MarketChange .
     fromJustNote "response: could not parse marketchange" .
     (decode :: L.ByteString -> Maybe M.MarketChangeMessage)) b
  | isJust (decode b :: Maybe O.OrderChangeMessage) && oop == "ocm" =
    (OrderChange .
     fromJustNote "response: could not parse orderchange" .
     (decode :: L.ByteString -> Maybe O.OrderChangeMessage)) b
  | isJust (decode b :: Maybe S.StatusMessage) && sop == "status" =
    (Status .
     fromJustNote "response: could not parse status" .
     (decode :: L.ByteString -> Maybe S.StatusMessage)) b
  | otherwise = error $ "response: could not parse bytestring" ++ show b
  where cop = (C.op . fromJustNote "") (decode b :: Maybe C.ConnectionMessage)
        mop = (M.op . fromJustNote "") (decode b :: Maybe M.MarketChangeMessage)
        oop = (O.op . fromJustNote "") (decode b :: Maybe O.OrderChangeMessage)
        sop = (S.op . fromJustNote "") (decode b :: Maybe S.StatusMessage)
