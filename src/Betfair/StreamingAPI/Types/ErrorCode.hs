{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.ErrorCode
  (ErrorCode(..))
  where

import BasicPrelude
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

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
  deriving (Eq,Show,Read)

deriveDefault ''ErrorCode

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''ErrorCode)
