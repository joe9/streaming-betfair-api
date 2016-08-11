{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Betfair.StreamingAPI.Types.RequestStatus
  (RequestStatus(..))
  where

import BasicPrelude
import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

data RequestStatus
  = SUCCESS
  | FAILURE
  deriving (Eq,Show,Read)

deriveDefault ''RequestStatus

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''RequestStatus)
