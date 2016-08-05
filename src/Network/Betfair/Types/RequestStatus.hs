{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Network.Betfair.Types.RequestStatus
  (RequestStatus(..))
  where

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
