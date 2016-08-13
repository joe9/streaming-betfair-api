{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.ResponseException
  (ResponseException(..))
  where

import BasicPrelude
--
import Betfair.StreamingAPI.API.Response
import qualified Betfair.APING as NG

data ResponseException
  = ParserError Text
  | EmptyLine Text
  | NotImplemented Response
                   (Maybe Text)
  | APINGResponseException NG.ResponseException
  deriving (Eq,Show)
