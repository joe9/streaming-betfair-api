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
import qualified Data.Map.Strict            as Map
import           Data.String.Conversions
import           Network.Connection
import           Safe
--
import           Betfair.StreamingAPI.API.Context
import           Betfair.StreamingAPI.API.Log
import           Betfair.StreamingAPI.API.Response
import           Betfair.StreamingAPI.API.ResponseException
import           Betfair.StreamingAPI.API.StreamingState
import           Betfair.StreamingAPI.API.MarketState
import qualified Betfair.StreamingAPI.Responses.MarketChangeMessage as M
import qualified Betfair.StreamingAPI.Responses.StatusMessage       as S
import           Betfair.StreamingAPI.Types.ChangeType
import qualified Betfair.StreamingAPI.Types.MarketChange            as MarketChange

-- import           Betfair.StreamingAPI.Types.MarketStatus
responseT
  :: (Context a) -> ExceptT ResponseException IO (Response,(Context a))
responseT c = ExceptT ( response c) >>= (\(r,cu) -> (cOnResponse cu) r cu)

response
  :: (Context a) -> IO (Either ResponseException (Response,(Context a)))
response c =
  do raw <-
       connectionGetLine 16384
                         (cConnection c)
     _ <- groomedLog c From raw
     groomedLog c
                From
                (parseResponse raw >>= processResponse c)

--      (return . Right) (c,undefined)
processResponse
  :: (Context a) -> Response -> Either ResponseException (Response,(Context a))
processResponse c r@(MarketChange m)
  | isNothing (M.ct m) || M.ct m == Just HEARTBEAT = Right (r,c)
  | isNothing (M.mc m) || M.mc m == Just [] = Right (r,c)
  | isJust (M.segmentType m) =
    notImplementedText
      r
      ("segmentType processing not implemented" <> (cs . show) m)
  | isNothing (M.segmentType m) =
    Right (r,c {cState =
                ((foldl (updateStreamingState (M.clk m)
                                              (M.initialClk m)
                                              (M.pt m))
                        (cState c) .
                  fromJustNote "should have markets here" . M.mc) m)}
          )
  | otherwise =
    notImplementedText r
                       ("processResponse: " <> (cs . show) m)
processResponse c (Status status _) =
  Right (Status status
                (Map.lookup (fromMaybe 0 (S.id status))
                            (ssRequests (cState c))),c)
processResponse c r@(Connection _) = Right (r,c)
processResponse _ r@(OrderChange _) = notImplemented r

-- processResponse c r = Right (r,c)
updateStreamingState :: Maybe Text
                     -> Maybe Text
                     -> Integer
                     -> StreamingState
                     -> MarketChange.MarketChange
                     -> StreamingState
updateStreamingState c i p s mc =
  (s {ssMarkets =
        (Map.insert (MarketChange.id mc)
                    u .
         ssMarkets) s})
  where m =
          (fromJustNote "updateMarket: " .
           Map.lookup (MarketChange.id mc) . ssMarkets) s
        u = updateMarket c i p m mc

updateMarket :: Maybe Text
             -> Maybe Text
             -> Integer
             -> MarketState
             -> MarketChange.MarketChange
             -> MarketState
updateMarket c i pt m _ = (s {msPublishTime = pt})
  where f = maybe m (\x -> m {msClk = Just x}) c
        s = maybe f (\x -> f {msInitialClk = Just x}) i

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
parserError = Left . ParserError
