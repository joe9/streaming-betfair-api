{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Betfair.API.Response
  (response)
  where

import           Control.Monad.RWS
import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Network.Connection
import           Safe

import qualified Network.Betfair.Responses.ConnectionMessage   as C
import qualified Network.Betfair.Responses.MarketChangeMessage as M
import qualified Network.Betfair.Responses.OrderChangeMessage  as O
import qualified Network.Betfair.Responses.StatusMessage       as S

import           Network.Betfair.API.Config
import           Network.Betfair.API.StreamingState
import           Network.Betfair.Types.ChangeType
import qualified Network.Betfair.Types.MarketChange as MarketChange
import           Network.Betfair.Types.MarketStatus

import WriterLog

data Response
  = Connection C.ConnectionMessage
  | MarketChange M.MarketChangeMessage
  | OrderChange O.OrderChangeMessage
  | Status S.StatusMessage

-- response :: RWST Connection Log s IO Response
-- response =
--   ask >>= lift . connectionGetLine 16384 >>= groomedLog >>=
--   lift . return . parseResponse . L.fromStrict

response
  :: RWST Config Log StreamingState IO Response
response =
  do state <- get
     raw <-
       (lift .
        connectionGetLine 16384 . fromJustNote "response: " . ssConnection) state
     _ <- groomedLog raw
     let response = (parseResponse . L.fromStrict) raw
     updatedState <- lift (processResponse state response)
     put updatedState
     return response

processResponse :: StreamingState -> Response -> IO StreamingState
processResponse s (OrderChange _) = return s -- not implemented
processResponse s (Connection _) = return s
processResponse s (Status _) = return s
processResponse s (MarketChange m)
  | isNothing (M.ct m) || M.ct m == Just HEARTBEAT = return s
  | isNothing (M.mc m) || M.mc m == Just [] = return s
  | isJust (M.segmentType m) =
    error $ "segmentType processing not implemented" ++ show m
  | isNothing (M.segmentType m) =
    (foldM (updateStreamingState (M.clk m)
                                 (M.initialClk m)
                                 (M.pt m))
           s .
     fromJustNote "should have markets here" . M.mc) m
  | otherwise = error $ "not implemented" ++ show m

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
        mop =
          (M.op . fromJustNote "") (decode b :: Maybe M.MarketChangeMessage)
        oop = (O.op . fromJustNote "") (decode b :: Maybe O.OrderChangeMessage)
        sop = (S.op . fromJustNote "") (decode b :: Maybe S.StatusMessage)

updateStreamingState :: Maybe String
                     -> Maybe String
                     -> Integer
                     -> StreamingState
                     -> MarketChange.MarketChange
                     -> IO StreamingState
updateStreamingState c i p s mc =
  do let m =
           (fromJustNote "updateMarket: " .
            Map.lookup (MarketChange.id mc) . ssMarkets) s
     u <- updateMarket c i p m mc
     return (s {ssMarkets =
                  (Map.insert (MarketChange.id mc)
                              u .
                   ssMarkets) s})

updateMarket :: Maybe String
             -> Maybe String
             -> Integer
             -> MarketState
             -> MarketChange.MarketChange
             -> IO MarketState
updateMarket c i pt m mc = processUpdatedMarket (s {msPublishTime = pt})
  where f = maybe m (\x -> m {msClk = Just x}) c
        s = maybe f (\x -> f {msInitialClk = Just x}) i

processUpdatedMarket
  :: MarketState -> IO MarketState
processUpdatedMarket = pure
