{-# OPTIONS_GHC -Wall #-}

module Network.Betfair.Requests.HeartbeatMessage (HeartbeatMessage(..)) where

import qualified Data.ByteString.Lazy                    as L
import           Network.HTTP.Conduit

data HeartbeatMessage = HeartbeatMessage
   { op :: String
   , id      :: Int
   } deriving (Eq, Show)

-- heartbeatRequest :: Int -> HeartbeatMessage
-- heartbeatRequest i = undefined
-- --  parseUrl "https://identitysso.betfair.com/api/keepAlive"
-- --    >>= (\req -> return $ req {requestHeaders = headers (Just t)})

-- heartbeat :: Token -> IO (Response L.ByteString)
-- heartbeat t = undefined -- heartbeatRequest t >>= getResponse
