{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Betfair.StreamingAPI.API.ResponseException
  (ResponseException(..))
  where

import BasicPrelude

data ResponseException
  = ParserError Text
  | EmptyLine Text
  | NotImplemented Text
  deriving (Eq,Show)
