{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Network.Betfair.Types.OrderStatus
  (OrderStatus(..))
  where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)
import Data.Text

-- Status - the status of the order (E = EXECUTABLE, EC = EXECUTION_COMPLETE)
data OrderStatus
  = E
  | EC
  deriving (Eq,Show,Read)

deriveDefault ''OrderStatus

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''OrderStatus)
