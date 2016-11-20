{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Betfair.StreamingAPI.Responses.StatusMessage
  ( StatusMessage(..)
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude
import Text.PrettyPrint.GenericPretty

--
import Betfair.StreamingAPI.Types.ErrorCode     (ErrorCode)
import Betfair.StreamingAPI.Types.RequestStatus (RequestStatus)

data StatusMessage = StatusMessage
  { op               :: Text
  , id               :: Maybe Integer -- Client generated unique id to link request with response (like json rpc)
  , errorMessage     :: Maybe Text -- The type of error in case of a failure
  , errorCode        :: Maybe ErrorCode -- The type of error in case of a failure
  , connectionId     :: Maybe Text -- The connection id
  , connectionClosed :: Maybe Bool -- Is the connection now closed
  , statusCode       :: RequestStatus -- The status of the last request
  } deriving (Eq, Read, Show, Generic, Pretty)

$(deriveJSON defaultOptions {omitNothingFields = True} ''StatusMessage)
