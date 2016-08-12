{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Betfair.StreamingAPI.API.StreamingState
  (StreamingState(..)
  ,ConnectionState(..)
  ,addMarketIds)
  where

import           BasicPrelude
import           Data.Default
import qualified Data.Map.Strict as Map
-- import           Data.Text
-- import Data.Maybe
--
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.MarketState
import Betfair.StreamingAPI.API.Request

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

addMarketIds
  :: StreamingState -> [MarketId] -> StreamingState
addMarketIds ss mids =
  ss {ssMarkets =
        Map.union (ssMarkets ss)
                  ((Map.fromList . fmap (\mid -> (mid,def {msMarketId = mid}))) mids)}
