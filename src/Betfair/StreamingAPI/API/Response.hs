{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Response
  (response
  ,Response(..))
  where

import           BasicPrelude
import           Control.Monad.RWS
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString                                    hiding
                                                                     (append)
import qualified Data.Map.Strict                                    as Map
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text
import           Network.Connection
import           Safe
--
import           Betfair.StreamingAPI.API.Context
import           Betfair.StreamingAPI.API.Log
import           Betfair.StreamingAPI.API.Request
import           Betfair.StreamingAPI.API.ResponseException
import           Betfair.StreamingAPI.API.StreamingState
import qualified Betfair.StreamingAPI.Responses.ConnectionMessage   as C
import qualified Betfair.StreamingAPI.Responses.MarketChangeMessage as M
import qualified Betfair.StreamingAPI.Responses.OrderChangeMessage  as O
import qualified Betfair.StreamingAPI.Responses.StatusMessage       as S
import           Betfair.StreamingAPI.Types.ChangeType
import qualified Betfair.StreamingAPI.Types.MarketChange            as MarketChange
-- import           Betfair.StreamingAPI.Types.MarketStatus

data Response
  = Connection C.ConnectionMessage
  | MarketChange M.MarketChangeMessage
  | OrderChange O.OrderChangeMessage
  | Status S.StatusMessage
           (Maybe Request)
  deriving (Eq,Read,Show)

-- response :: RWST Context l s IO Response
-- response =
--   ask >>= lift . connectionGetLine 16384 >>= groomedLog >>=
--   lift . return . parseResponse . L.fromStrict
response
  :: Context -> IO (Either ResponseException Response)
response =
  do
     raw <- lift (connectionGetLine 16384 (cConnection c))
     _ <- groomedLog From raw
     (\r -> groomedLog From =<< processResponse r) (parseResponse raw)

processResponse
  :: Context -> Response -> IO (Either ResponseException Response)
processResponse c r@(OrderChange _) = return r -- not implemented
processResponse c r@(Connection _) = return r
processResponse c (Status status _) =
  do s <- get
     return (Status status
                    (Map.lookup (fromMaybe 0 (S.id status))
                                (ssRequests s)))
processResponse c r@(MarketChange m)
  | isNothing (M.ct m) || M.ct m == Just HEARTBEAT = return r
  | isNothing (M.mc m) || M.mc m == Just [] = return r
  | isJust (M.segmentType m) =
    return (NotImplemented
              (append "segmentType processing not implemented" (Data.Text.pack (show m))))
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
  | otherwise =
    return (NotImplemented (append "processResponse: " (Data.Text.pack (show m))))
processResponse c r = return r

-- processResponse r@(EmptyLine) = return r
-- processResponse r@(NotImplemented _) = return r
-- processResponse r@(JSONParseError _) = return r
updateStreamingState :: Maybe Text
                     -> Maybe Text
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

updateMarket :: Maybe Text
             -> Maybe Text
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

opIs :: Object -> Either Text Text
opIs = either (Left . cs) Right . parseEither (flip (.:) "op")

responseIs
  :: ByteString -> Text -> Either Text Response
responseIs b op
  | op == "mcm" = eitherDecodeStrictText b >>= (\r -> Right (MarketChange r))
  | op == "ocm" = eitherDecodeStrictText b >>= (\r -> Right (OrderChange r))
  | op == "connection" =
    eitherDecodeStrictText b >>= (\r -> Right (Connection r))
  | op == "status" =
    eitherDecodeStrictText b >>= (\r -> Right (Status r Nothing))
  | b == "" = Right EmptyLine
  | b == "\n" =
    Right (NotImplemented (append "response: received newline only: " (cs b)))
  | b == "\r" =
    Right (NotImplemented (append "response: received carriage return only: " (cs b)))
  | b == "\r\n" =
    Right (NotImplemented (append "response: received CRLF only: " (cs b)))
  | otherwise =
    Right (NotImplemented (append "response: could not parse bytestring: " (cs b)))

parseResponse :: ByteString -> Response
parseResponse b =
  either JSONParseError id (eitherDecodeStrictText b >>= opIs >>= responseIs b)

eitherDecodeStrictText
  :: FromJSON a
  => ByteString -> Either Text a
eitherDecodeStrictText = either (Left . cs) Right . eitherDecodeStrict
