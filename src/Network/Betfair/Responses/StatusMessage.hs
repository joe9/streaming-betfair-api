{-# OPTIONS_GHC -Wall            #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Betfair.Responses.StatusMessage
  (StatusMessage(..))
  where

import Data.Aeson.TH
       (Options(omitNothingFields), defaultOptions, deriveJSON)

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

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''StatusMessage)
