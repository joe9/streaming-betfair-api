{-# OPTIONS_GHC -Wall            #-}

module Network.Betfair.Responses.StatusMessage
  (StatusMessage(..))
  where

import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Conduit

import Network.Betfair.Types.ErrorCode   (ErrorCode)
import Network.Betfair.Types.RequestStatus (RequestStatus)

data StatusMessage =
  StatusMessage {op               :: String
                ,id               :: Integer -- Client generated unique id to link request with response (like json rpc)
                ,errorCode        :: ErrorCode -- The type of error in case of a failure
                ,connectionId     :: String -- The connection id
                ,connectionClosed :: Bool -- Is the connection now closed
                ,statusCode       :: RequestStatus -- The status of the last request
                }
  deriving (Eq,Show)

-- heartbeatRequest :: Int -> StatusMessage
-- heartbeatRequest i = undefined
-- --  parseUrl "https://identitysso.betfair.com/api/keepAlive"
-- --    >>= (\req -> return $ req {requestHeaders = headers (Just t)})

-- heartbeat :: Token -> IO (Response L.ByteString)
-- heartbeat t = undefined -- heartbeatRequest t >>= getResponse
