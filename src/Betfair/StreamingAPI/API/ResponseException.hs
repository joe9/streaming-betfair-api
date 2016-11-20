{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.ResponseException
  ( ResponseException(..)
  ) where

import Text.PrettyPrint.GenericPretty
import Protolude

--
import Betfair.StreamingAPI.API.Response

data ResponseException
  = ParserError Text
  | EmptyLine Text
  | NotImplemented Response
                   (Maybe Text)
  deriving (Eq, Show, Generic, Pretty, Typeable)

instance Exception ResponseException
