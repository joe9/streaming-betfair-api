{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.ResponseProcessing
  ( response
  , Response(..)
  ) where

import           BasicPrelude
import           Control.Exception.Safe
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.IntMap.Strict      as IntMap
import           Data.String.Conversions
import           Network.Connection

-- import           Safe
--
import           Betfair.StreamingAPI.API.Context
import           Betfair.StreamingAPI.API.Log
import           Betfair.StreamingAPI.API.Request
import           Betfair.StreamingAPI.API.Response
import           Betfair.StreamingAPI.API.ResponseException
import           Betfair.StreamingAPI.API.StreamingState
import qualified Betfair.StreamingAPI.Requests.MarketSubscriptionMessage as MS
import qualified Betfair.StreamingAPI.Requests.OrderSubscriptionMessage  as OS
import qualified Betfair.StreamingAPI.Responses.MarketChangeMessage      as M
import qualified Betfair.StreamingAPI.Responses.StatusMessage            as S
import           Betfair.StreamingAPI.Types.ChangeType

-- import qualified Betfair.StreamingAPI.Types.MarketChange            as MarketChange
-- import           Betfair.StreamingAPI.Types.MarketStatus
response :: Context -> IO (Maybe MS.MarketSubscriptionMessage, Context)
response c = do
  raw <- connectionGetLine 268435456 (cConnection c)
  _ <- groomedLog c From raw
  let eitherResponse = parseResponse raw >>= processResponse c
  case eitherResponse of
    Left e -> throwM e
    Right (r, cu) -> (groomedLog cu From) r >> (cOnResponse cu) r cu

--      (return . Right) (c,undefined)
processResponse :: Context
                -> Response
                -> Either ResponseException (Response, Context)
processResponse c r@(MarketChange m)
  | M.ct m == Just HEARTBEAT = Right (r, c)
  | isNothing (M.segmentType m) && isJust (M.mc m) =
    Right
      ( r
      , c
        { cState =
            (cState c)
            { ssRequests =
                updateClks
                  (M.clk m)
                  (M.initialClk m)
                  (M.id m)
                  ((ssRequests . cState) c)
            }
        })
  | isJust (M.segmentType m) =
    notImplementedText
      r
      ("segmentType processing not implemented" <> (cs . show) m)
  | otherwise = notImplementedText r ("processResponse: " <> (cs . show) m)
processResponse c (Status status _) =
  Right
    ( Status
        status
        (S.id status >>=
         (\i -> IntMap.lookup (fromIntegral i) (ssRequests (cState c))))
    , c)
processResponse c r@(Connection _) = Right (r, c)
processResponse _ r@(OrderChange _) = notImplemented r

updateClks
  :: Maybe Text -- clk
  -> Maybe Text -- initialClk
  -> Integer -- Request Id from MarketChange.id
  -> IntMap.IntMap Request
  -> IntMap.IntMap Request
updateClks Nothing Nothing _ mrs = mrs
updateClks c i rid mrs =
  IntMap.insert
    (fromIntegral rid)
    (updateRequestClks c i (IntMap.lookup (fromIntegral rid) mrs))
    mrs

updateRequestClks
  :: Maybe Text -- clk
  -> Maybe Text -- initialClk
  -> Maybe Request
  -> Request
updateRequestClks c i Nothing = UnknownRequest c i
updateRequestClks _ _ (Just r@(Heartbeat _)) = r
updateRequestClks _ _ (Just r@(Authentication _)) = r
updateRequestClks c i (Just (MarketSubscribe m)) =
  MarketSubscribe
    (m {MS.initialClk = i <|> MS.initialClk m, MS.clk = c <|> MS.clk m})
updateRequestClks c i (Just (OrderSubscribe m)) =
  OrderSubscribe
    (m {OS.initialClk = i <|> OS.initialClk m, OS.clk = c <|> OS.clk m})
updateRequestClks c i (Just (UnknownRequest oc oi)) =
  UnknownRequest (i <|> oi) (c <|> oc)

opIs :: Object -> Either ResponseException Text
opIs = either (Left . ParserError . cs) Right . parseEither (flip (.:) "op")

responseIs :: ByteString -> Text -> Either ResponseException Response
responseIs b op
  | op == "mcm" =
    eitherDecodeStrictResponseException b >>= (Right . MarketChange)
  | op == "ocm" =
    eitherDecodeStrictResponseException b >>= (Right . OrderChange)
  | op == "connection" =
    eitherDecodeStrictResponseException b >>= (Right . Connection)
  | op == "status" =
    eitherDecodeStrictResponseException b >>= (\r -> Right (Status r Nothing))
  | otherwise = parserError ("response: could not parse bytestring: " <> cs b)

parseResponse :: ByteString -> Either ResponseException Response
parseResponse b
  | b == "" = (Left . EmptyLine . cs) b
  | b == "\n" = parserError ("response: received newline only: " <> cs b)
  | b == "\r" =
    parserError ("response: received carriage return only: " <> cs b)
  | b == "\r\n" = parserError ("response: received CRLF only: " <> cs b)
  | otherwise = eitherDecodeStrictResponseException b >>= opIs >>= responseIs b

eitherDecodeStrictResponseException
  :: FromJSON a
  => ByteString -> Either ResponseException a
eitherDecodeStrictResponseException b =
  case eitherDecodeStrict b of
    Left e  -> (Left . ParserError . cs) e
    Right a -> Right a

notImplemented :: Response -> Either ResponseException b
notImplemented r = Left (NotImplemented r Nothing)

notImplementedText :: Response -> Text -> Either ResponseException b
notImplementedText r t = Left (NotImplemented r (Just t))

parserError :: Text -> Either ResponseException b
parserError = Left . ParserError -- if isNull unprocessed
--   then unprocessed <- recv
--        context {unprocessed = unprocessed}
--        recurse
--   else  if has suffix of \r \n
--         then context {unprocessed = snd . break above}
--              processResponse . fst . breakSubstring \r\n
--         else unprocessed <- recv
--              context {unprocessed = unprocessed}
--              recurse
