{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.ResponseException
  (ResponseException(..))
  where

import BasicPrelude
--
import Betfair.StreamingAPI.API.Response

data ResponseException
  = ParserError Text
  | EmptyLine Text
  | NotImplemented Response
                   (Maybe Text)
  deriving (Eq,Show,Typeable)

instance Exception ResponseException
