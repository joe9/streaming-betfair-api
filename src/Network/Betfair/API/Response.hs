{-# LANGUAGE OverloadedStrings #-}

module Network.Betfair.API.Response
  (response
  ,Response(..))
  where

import           Control.Monad.RWS
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString hiding (append)
import qualified Data.Map.Strict                               as Map
import           Data.Maybe
import           Data.Text
import           Data.String.Conversions
import           Network.Betfair.API.Context
import           Network.Betfair.API.Log
import           Network.Betfair.API.Request
import           Network.Betfair.API.StreamingState
import qualified Network.Betfair.Responses.ConnectionMessage   as C
import qualified Network.Betfair.Responses.MarketChangeMessage as M
import qualified Network.Betfair.Responses.OrderChangeMessage  as O
import qualified Network.Betfair.Responses.StatusMessage       as S
import           Network.Betfair.Types.ChangeType
import qualified Network.Betfair.Types.MarketChange            as MarketChange
import           Network.Connection
import           Safe

-- import           Network.Betfair.Types.MarketStatus
data Response
  = Connection C.ConnectionMessage
  | MarketChange M.MarketChangeMessage
  | OrderChange O.OrderChangeMessage
  | Status S.StatusMessage
           (Maybe Request)
  | EmptyLine
  | NotImplemented Text
  | JSONParseError Text
  deriving (Eq,Read,Show)

-- response :: RWST Context l s IO Response
-- response =
--   ask >>= lift . connectionGetLine 16384 >>= groomedLog >>=
--   lift . return . parseResponse . L.fromStrict
response
  :: RWST Context () StreamingState IO Response
response =
  do
     -- state <- get
     connection <- fmap cConnection ask
     raw <- lift (connectionGetLine 16384 connection)
     _ <- groomedLog From raw
     (\r -> groomedLog From =<< processResponse r) (parseResponse raw)

processResponse
  :: Response -> RWST Context () StreamingState IO Response
processResponse r@(OrderChange _) = return r -- not implemented
processResponse r@(Connection _) = return r
processResponse (Status status _) =
  do s <- get
     return (Status status
                    (Map.lookup (fromMaybe 0 (S.id status))
                                (ssRequests s)))
processResponse r@(MarketChange m)
  | isNothing (M.ct m) || M.ct m == Just HEARTBEAT = return r
  | isNothing (M.mc m) || M.mc m == Just [] = return r
  | isJust (M.segmentType m) =
    return (NotImplemented (append "segmentType processing not implemented" (Data.Text.pack ( show m))))
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
  | otherwise = return (NotImplemented (append "processResponse: " (Data.Text.pack (show m))))
processResponse r = return r

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
  | op == "connection" = eitherDecodeStrictText b >>= (\r -> Right (Connection r))
  | op == "status" = eitherDecodeStrictText b >>= (\r -> Right (Status r Nothing))
  | b == "" = Right EmptyLine
  | b == "\n" =
    Right (NotImplemented (append "response: received newline only: " (cs b)))
  | b == "\r" =
    Right (NotImplemented
             (append "response: received carriage return only: " (cs b)))
  | b == "\r\n" =
    Right (NotImplemented (append "response: received CRLF only: " (cs b)))
  | otherwise =
    Right (NotImplemented (append "response: could not parse bytestring: " (cs b)))

parseResponse :: ByteString -> Response
parseResponse b =
  either JSONParseError id (eitherDecodeStrictText b >>= opIs >>= responseIs b)

eitherDecodeStrictText :: FromJSON a => ByteString -> Either Text a
eitherDecodeStrictText = either (Left . cs) Right . eitherDecodeStrict
