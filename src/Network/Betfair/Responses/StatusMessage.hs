{-# OPTIONS_GHC -Wall            #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Betfair.Responses.StatusMessage
  (StatusMessage(..))
  where

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)

import Network.Betfair.Types.ErrorCode     (ErrorCode)
import Network.Betfair.Types.RequestStatus (RequestStatus)

data StatusMessage =
  StatusMessage {op               :: Text
                ,id               :: Maybe Integer -- Client generated unique id to link request with response (like json rpc)
                ,errorMessage     :: Maybe Text -- The type of error in case of a failure
                ,errorCode        :: Maybe ErrorCode -- The type of error in case of a failure
                ,connectionId     :: Maybe Text -- The connection id
                ,connectionClosed :: Maybe Bool -- Is the connection now closed
                ,statusCode       :: RequestStatus -- The status of the last request
                }
  deriving (Eq,Read,Show)

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''StatusMessage)
