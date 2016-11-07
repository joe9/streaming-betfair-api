{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- http://stackoverflow.com/questions/27591266/telling-cabal-where-the-main-module-is
module Betfair.StreamingAPI.API.CommonTypes where

import Protolude

type MarketId = Text

type MarketName = Text

type EventName = Text

type SessionToken = Text

type AppKey = Text
