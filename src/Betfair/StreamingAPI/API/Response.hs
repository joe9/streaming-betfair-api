{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Betfair.StreamingAPI.API.Response
  ( Response(..)
  ) where

import Text.PrettyPrint.GenericPretty
import Protolude
import Data.Aeson

import           Betfair.StreamingAPI.API.Request
import qualified Betfair.StreamingAPI.Responses.ConnectionMessage   as C
import qualified Betfair.StreamingAPI.Responses.MarketChangeMessage as M
import qualified Betfair.StreamingAPI.Responses.OrderChangeMessage  as O
import qualified Betfair.StreamingAPI.Responses.StatusMessage       as S

data Response
  = Connection C.ConnectionMessage
  | MarketChange M.MarketChangeMessage
  | OrderChange O.OrderChangeMessage
  | Status S.StatusMessage
           (Maybe Request)
  deriving (Eq, Read, Show, Generic, Pretty)
