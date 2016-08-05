{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Betfair.API.Response
  (response
  ,Response(..))
  where

import           Control.Monad.RWS
import           Data.Aeson
import           Data.Text
import           Data.Aeson.Types
import           Data.ByteString
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import           Network.Connection
import           Safe

import qualified Network.Betfair.Responses.ConnectionMessage   as C
import qualified Network.Betfair.Responses.MarketChangeMessage as M
import qualified Network.Betfair.Responses.OrderChangeMessage  as O
import qualified Network.Betfair.Responses.StatusMessage       as S

import           Network.Betfair.API.Context
import           Network.Betfair.API.Log
import           Network.Betfair.API.Request
import           Network.Betfair.API.StreamingState
import           Network.Betfair.Types.ChangeType
import qualified Network.Betfair.Types.MarketChange as MarketChange
-- import           Network.Betfair.Types.MarketStatus

data Response
  = Connection C.ConnectionMessage
  | MarketChange M.MarketChangeMessage
  | OrderChange O.OrderChangeMessage
  | Status S.StatusMessage
           (Maybe Request)
  | EmptyLine
  | NotImplemented String
  | JSONParseError String
  deriving (Eq,Read,Show)

-- response :: RWST Context l s IO Response
-- response =
--   ask >>= lift . connectionGetLine 16384 >>= groomedLog >>=
--   lift . return . parseResponse . L.fromStrict

response
  :: RWST Context () StreamingState IO Response
response =
  do -- state <- get
     connection <- fmap cConnection ask
     raw <- lift (connectionGetLine 16384 connection)
     _ <- groomedLog From raw
     (\r -> groomedLog From =<< processResponse r) (parseResponse raw)

processResponse
  :: Response -> RWST Context () StreamingState IO Response
processResponse r@(OrderChange _) = return r -- not implemented
processResponse r@(Connection _) = return r
processResponse   (Status status _) =
  do s <- get
     return (Status status
                    (Map.lookup (fromMaybe 0 (S.id status))
                                (ssRequests s)))
processResponse r@(MarketChange m)
  | isNothing (M.ct m) || M.ct m == Just HEARTBEAT = return r
  | isNothing (M.mc m) || M.mc m == Just [] = return r
  | isJust (M.segmentType m) =
    return (NotImplemented ("segmentType processing not implemented" ++ show m))
  | isNothing (M.segmentType m) =
    do s <- get
       u <-
         lift ((foldM (updateStreamingState (M.clk m)
                                            (M.initialClk m)
                                            (M.pt m))
                      s .
                fromJustNote "should have markets here" . M.mc) m)
       put u
       return r
  | otherwise = return (NotImplemented ("processResponse: " ++ show m))
processResponse r = return r
-- processResponse r@(EmptyLine) = return r
-- processResponse r@(NotImplemented _) = return r
-- processResponse r@(JSONParseError _) = return r

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
updateMarket c i pt m _ = processUpdatedMarket (s {msPublishTime = pt})
  where f = maybe m (\x -> m {msClk = Just x}) c
        s = maybe f (\x -> f {msInitialClk = Just x}) i

processUpdatedMarket
  :: MarketState -> IO MarketState
processUpdatedMarket = pure

opIs :: Object -> Either String String
opIs = parseEither (flip (.:) "op")

responseIs
  :: ByteString -> String -> Either String Response
responseIs b op
  | op == "mcm" = eitherDecodeStrict b >>= (\r -> Right (MarketChange r))
  | op == "ocm" = eitherDecodeStrict b >>= (\r -> Right (OrderChange r))
  | op == "connection" = eitherDecodeStrict b >>= (\r -> Right (Connection r))
  | op == "status" = eitherDecodeStrict b >>= (\r -> Right (Status r Nothing))
  | b == "" = Right EmptyLine
  | b == "\n" =
    Right (NotImplemented ("response: received newline only: " ++ show b))
  | b == "\r" =
    Right (NotImplemented
             ("response: received carriage return only: " ++ show b))
  | b == "\r\n" =
    Right (NotImplemented ("response: received CRLF only: " ++ show b))
  | otherwise =
    Right (NotImplemented ("response: could not parse bytestring: " ++ show b))

parseResponse :: ByteString -> Response
parseResponse b =
  either JSONParseError id (eitherDecodeStrict b >>= opIs >>= responseIs b)
