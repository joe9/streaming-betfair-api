{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Betfair.StreamingAPI.API.StreamingState
  (StreamingState(..)
  ,MarketState(..)
  ,ConnectionState(..)
  ,MarketConnectionState(..)
  ,MarketId
  ,MarketName
  ,EventName)
  where

import           BasicPrelude
import           Data.Default
import qualified Data.Map.Strict as Map
-- import           Data.Text
-- import Data.Maybe
--
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Request
import Betfair.StreamingAPI.Types.MarketStatus

data StreamingState =
  StreamingState {ssMarkets         :: Map.Map MarketId MarketState
                 ,ssRequests        :: Map.Map Integer Request
                 ,ssIdCounter       :: Integer
                 ,ssSessionToken    :: SessionToken
                 ,ssAppKey          :: AppKey
                 ,ssConnectionState :: ConnectionState
                 ,ssNeedHumanHelp   :: Bool}
  deriving (Eq,Read,Show)

instance Default StreamingState where
  def = StreamingState Map.empty Map.empty 1 "" "" NotConnected False

data ConnectionState
  = NotConnected
  | NotAuthenticated
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
              ,msInitialClk      :: Maybe Text
              ,msClk             :: Maybe Text
              ,msPublishTime     :: Integer}
  deriving (Eq,Read,Show)

instance Default MarketState where
  def = MarketState Nothing "" "" False "" ToSubscribe Nothing Nothing 0
