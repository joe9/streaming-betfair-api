{-# OPTIONS_GHC -Wall       #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Network.Betfair.API.StreamingState
  (StreamingState(..)
  ,MarketState(..)
  ,MarketId
  ,MarketName
  ,EventName)
  where

import Data.Default
-- import Data.Maybe
import qualified Data.Map.Strict    as Map

import Network.Betfair.API.Request
import Network.Betfair.API.Config
import Network.Betfair.Types.MarketStatus

type MarketId = String
type MarketName = String
type EventName = String
type SessionToken = String
data StreamingState =
  StreamingState {ssMarkets         :: Map.Map MarketId MarketState
                 ,ssRequests        :: Map.Map Integer Request
                 ,ssIdCounter       :: Integer
                 ,ssSessionToken    :: SessionToken
                 ,ssConfig          :: Config
                 ,ssConnectionState :: ConnectionState}
  deriving (Eq,Read,Show)

data ConnectionState
  = NotConnected
  | ConnectSent
  | Connected
  | AuthenticateSent
  | Authenticated
  | ReceivingData
  deriving (Eq,Read,Show)

data MarketConnectionState
  = ToSubscribe
  | SubscribeSent
  | Subscribed
  | MarketChangeReceived
  | RemoveReceived
  deriving (Eq,Read,Show)

data MarketState =
  MarketState {msStatus          :: Maybe MarketStatus
              ,msMarketName      :: MarketName
              ,msEventName       :: EventName
              ,msStateChanged    :: Bool
              ,msMarketId        :: MarketId
              ,msConnectionState :: MarketConnectionState
              ,msInitialClk      :: Maybe String
              ,msClk             :: Maybe String
              ,msPublishTime     :: Integer}
  deriving (Eq,Read,Show)

instance Default StreamingState where
  def = StreamingState Map.empty Map.empty 1 "" def NotConnected
