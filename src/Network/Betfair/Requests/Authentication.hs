{-# OPTIONS_GHC -Wall    #-}

module Network.Betfair.Requests.Authentication
  (Authentication(..))
  where

import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Conduit

data Authentication =
  Authentication {op      :: String
                 ,id      :: Integer -- Client generated unique id to link request with response (like json rpc)
                 ,session :: String
                 ,appKey  :: String}
  deriving (Eq,Show)

-- heartbeatRequest :: Int -> Authentication
-- heartbeatRequest i = undefined
-- --  parseUrl "https://identitysso.betfair.com/api/keepAlive"
-- --    >>= (\req -> return $ req {requestHeaders = headers (Just t)})

-- heartbeat :: Token -> IO (Response L.ByteString)
-- heartbeat t = undefined -- heartbeatRequest t >>= getResponse
