{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Betfair.StreamingAPI.API.MarketState
  (MarketState(..)
  ,MarketConnectionState(..))
  where

--   ,addMarketIds
import           BasicPrelude
import           Data.Default
import qualified Data.Map.Strict as Map
-- import           Data.Text
-- import Data.Maybe
--
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Request
import Betfair.StreamingAPI.Types.MarketStatus

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

data MarketConnectionState
  = ToSubscribe
  | SubscribeSent
  | Subscribed
  | MarketChangeReceived
  | RemoveReceived
  deriving (Eq,Read,Show)-- addMarketIds
                         --   :: Map MarketId MarketState -> [MarketId] -> StreamingState
                         -- addMarketIds ss mids = undefined
                         --   ss {ssMarkets =
                         --         Map.union (ssMarkets ss)
                         --                   ((Map.fromList . fmap (\mid -> (mid,def {msMarketId = mid}))) mids)}
