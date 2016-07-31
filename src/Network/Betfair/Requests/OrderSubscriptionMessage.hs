{-# OPTIONS_GHC -Wall         #-}

module Network.Betfair.Requests.OrderSubscriptionMessage
  (OrderSubscriptionMessage(..))
  where

import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Conduit

import Network.Betfair.Types.OrderFilter (OrderFilter)

data OrderSubscriptionMessage =
  OrderSubscriptionMessage {op                  :: String
                    ,id                  :: Integer -- Client generated unique id to link request with response (like json rpc)
                    ,segmentationEnabled :: Bool -- Segmentation Enabled - allow the server to send large sets of data in segments, instead of a single block
                    ,orderFilter         :: OrderFilter
                    ,clk                 :: String -- Token value delta (received in MarketChangeMessage) that should be passed to resume a subscription
                    ,heartbeatMs         :: Integer -- Heartbeat Milliseconds - the heartbeat rate (looped back on initial image after validation: bounds are 500 to 30000)
                    ,initialClk          :: String -- Token value (received in initial MarketChangeMessage) that should be passed to resume a subscription
                    ,conflateMs          :: Integer -- Conflate Milliseconds - the conflation rate (looped back on initial image after validation: bounds are 0 to 120000)
                    }
  deriving (Eq,Show)

-- heartbeatRequest :: Int -> OrderSubscriptionMessage
-- heartbeatRequest i = undefined
-- --  parseUrl "https://identitysso.betfair.com/api/keepAlive"
-- --    >>= (\req -> return $ req {requestHeaders = headers (Just t)})

-- heartbeat :: Token -> IO (Response L.ByteString)
-- heartbeat t = undefined -- heartbeatRequest t >>= getResponse
