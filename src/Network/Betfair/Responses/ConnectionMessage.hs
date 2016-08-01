{-# OPTIONS_GHC -Wall           #-}

module Network.Betfair.Responses.ConnectionMessage
  (ConnectionMessage(..))
  where

import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Conduit

data ConnectionMessage =
  ConnectionMessage {op           :: String
                    ,id           :: Integer -- Client generated unique id to link request with response (like json rpc)
                    ,connectionId :: String -- The connection id
                    }
  deriving (Eq,Show)

-- heartbeatRequest :: Int -> ConnectionMessage
-- heartbeatRequest i = undefined
-- --  parseUrl "https://identitysso.betfair.com/api/keepAlive"
-- --    >>= (\req -> return $ req {requestHeaders = headers (Just t)})

-- heartbeat :: Token -> IO (Response L.ByteString)
-- heartbeat t = undefined -- heartbeatRequest t >>= getResponse
