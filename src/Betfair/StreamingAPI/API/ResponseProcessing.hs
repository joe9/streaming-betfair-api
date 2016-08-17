{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.ResponseProcessing
  (response
  ,responseT
  ,Response(..))
  where

import           BasicPrelude
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict            as HashMap
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
import qualified Betfair.StreamingAPI.Responses.MarketChangeMessage as M
import qualified Betfair.StreamingAPI.Requests.MarketSubscriptionMessage as MS
import qualified Betfair.StreamingAPI.Requests.OrderSubscriptionMessage as OS
import qualified Betfair.StreamingAPI.Responses.StatusMessage       as S
import           Betfair.StreamingAPI.Types.ChangeType

-- import qualified Betfair.StreamingAPI.Types.MarketChange            as MarketChange
-- import           Betfair.StreamingAPI.Types.MarketStatus
responseT
  :: (Context a) -> ExceptT ResponseException IO (Context a)
responseT c = ExceptT (response c) >>= (\(r,cu) -> (cOnResponse cu) r cu)

response
  :: (Context a) -> IO (Either ResponseException (Response,(Context a)))
response c =
  do raw <-
       connectionGetLine 268435456
                         (cConnection c)
     _ <- groomedLog c From raw
     groomedLog c
                From
                (parseResponse raw >>= processResponse c)

--      (return . Right) (c,undefined)
processResponse
  :: (Context a) -> Response -> Either ResponseException (Response,(Context a))
processResponse c r@(MarketChange m)
  | M.ct m == Just HEARTBEAT = Right (r,c)
  | isNothing (M.segmentType m) && isJust (M.mc m) =
    Right (r
          ,c {cState =
                (cState c) {ssRequests =
                              updateClks (M.clk m)
                                         (M.initialClk m)
                                         (M.id m)
                                         ((ssRequests . cState) c)}})
  | isJust (M.segmentType m) =
    notImplementedText
      r
      ("segmentType processing not implemented" <> (cs . show) m)
  | otherwise =
    notImplementedText r
                       ("processResponse: " <> (cs . show) m)
processResponse c (Status status _) =
  Right (Status status
                (S.id status >>=
                 (\i -> (HashMap.lookup i
                                     (ssRequests (cState c)))))
        ,c)
processResponse c r@(Connection _) = Right (r,c)
processResponse _ r@(OrderChange _) = notImplemented r

updateClks
  :: Maybe Text -- clk
  -> Maybe Text -- initialClk
  -> Integer -- Request Id from MarketChange.id
  -> HashMap.HashMap Integer Request
  -> HashMap.HashMap Integer Request
updateClks Nothing Nothing _ mrs = mrs
updateClks c i rid mrs =
  HashMap.insert
    rid
    (updateRequestClks c
                       i
                       (HashMap.lookup rid mrs))
    mrs

updateRequestClks :: Maybe Text -- clk
                  -> Maybe Text -- initialClk
                  -> Maybe Request
                  -> Request
updateRequestClks c i Nothing = (UnknownRequest c i)
updateRequestClks _ _ (Just r@(Heartbeat _)) = r
updateRequestClks _ _ (Just r@(Authentication _)) = r
updateRequestClks c i (Just   (MarketSubscribe m)) =
  MarketSubscribe (m {MS.initialClk = maybe (MS.initialClk m) Just i
                     ,MS.clk = maybe (MS.clk m) Just c})
updateRequestClks c i (Just   (OrderSubscribe m)) =
  OrderSubscribe (m {OS.initialClk = maybe (OS.initialClk m) Just i
                     ,OS.clk = maybe (OS.clk m) Just c})
updateRequestClks c i (Just   (UnknownRequest oc oi)) =
  UnknownRequest (maybe oi Just i) (maybe oc Just c)

opIs :: Object -> Either ResponseException Text
opIs = either (Left . ParserError . cs) Right . parseEither (flip (.:) "op")

responseIs
  :: ByteString -> Text -> Either ResponseException Response
responseIs b op
  | op == "mcm" =
    eitherDecodeStrictResponseException b >>= (\r -> Right (MarketChange r))
  | op == "ocm" =
    eitherDecodeStrictResponseException b >>= (\r -> Right (OrderChange r))
  | op == "connection" =
    eitherDecodeStrictResponseException b >>= (\r -> Right (Connection r))
  | op == "status" =
    eitherDecodeStrictResponseException b >>= (\r -> Right (Status r Nothing))
  | otherwise =
    parserError ("response: could not parse bytestring: " <> (cs b))

parseResponse
  :: ByteString -> Either ResponseException Response
parseResponse b
  | b == "" = (Left . EmptyLine . cs) b
  | b == "\n" = parserError ("response: received newline only: " <> (cs b))
  | b == "\r" =
    parserError ("response: received carriage return only: " <> (cs b))
  | b == "\r\n" = parserError ("response: received CRLF only: " <> (cs b))
  | otherwise = eitherDecodeStrictResponseException b >>= opIs >>= responseIs b

eitherDecodeStrictResponseException
  :: FromJSON a
  => ByteString -> Either ResponseException a
eitherDecodeStrictResponseException b =
  case eitherDecodeStrict b of
    Left e  -> (Left . ParserError . cs) e
    Right a -> Right a

notImplemented
  :: Response -> Either ResponseException b
notImplemented r = Left (NotImplemented r Nothing)

notImplementedText
  :: Response -> Text -> Either ResponseException b
notImplementedText r t =
  Left (NotImplemented r
                       (Just t))

parserError :: Text -> Either ResponseException b
parserError = Left . ParserError-- if isNull unprocessed
                                --   then unprocessed <- recv
                                --        context {unprocessed = unprocessed}
                                --        recurse
                                --   else  if has suffix of \r \n
                                --         then context {unprocessed = snd . break above}
                                --              processResponse . fst . breakSubstring \r\n
                                --         else unprocessed <- recv
                                --              context {unprocessed = unprocessed}
                                --              recurse
