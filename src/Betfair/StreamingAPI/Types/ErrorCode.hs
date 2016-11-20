{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.ErrorCode
  ( ErrorCode(..)
  ) where

import Text.PrettyPrint.GenericPretty
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Protolude

-- The type of error in case of a failure
data ErrorCode
  = NO_APP_KEY
  | INVALID_APP_KEY
  | NO_SESSION
  | INVALID_SESSION_INFORMATION
  | NOT_AUTHORIZED
  | INVALID_INPUT
  | INVALID_CLOCK
  | UNEXPECTED_ERROR
  | TIMEOUT
  | SUBSCRIPTION_LIMIT_EXCEEDED
  | INVALID_REQUEST
  | CONNECTION_FAILED
  | MAX_CONNECTION_LIMIT_EXCEEDED
  deriving (Eq, Show, Generic, Pretty, Read)

$(deriveJSON defaultOptions {omitNothingFields = True} ''ErrorCode)

-- instance Pretty ErrorCode where
--   pretty NO_APP_KEY                             = No_App_Key
--   pretty INVALID_APP_KEY                        = Invalid_App_Key
--   pretty NO_SESSION                             = No_Session
--   pretty INVALID_SESSION_INFORMATION            = Invalid_Session_Information
--   pretty NOT_AUTHORIZED                         = Not_Authorized
--   pretty INVALID_INPUT                          = Invalid_Input
--   pretty INVALID_CLOCK                          = Invalid_Clock
--   pretty UNEXPECTED_ERROR                       = Unexpected_Error
--   pretty TIMEOUT                                = Timeout
--   pretty SUBSCRIPTION_LIMIT_EXCEEDED            = Subscription_Limit_Exceeded
--   pretty INVALID_REQUEST                        = Invalid_Request
--   pretty CONNECTION_FAILED                      = Connection_Failed
--   pretty MAX_CONNECTION_LIMIT_EXCEEDED          = Max_Connection_Limit_Exceeded
