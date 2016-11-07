{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Log
  ( Log
  , Direction(..)
  , toLog
  , logD
  , groomedLog
  , stdOutAndLog
  ) where

import Data.String.Conversions (cs)
import GHC.Show
import Protolude               hiding (show)
import Text.Groom              (groom)

import Betfair.StreamingAPI.API.Context

type Log = Text

data Direction
  = From
  | To
  | None

logD :: Context -> Direction -> Text -> IO ()
logD c d s = toLog c ((cs . show) d <> s)

toLog :: Context -> Text -> IO ()
toLog c = cLogger c

groomedLog
  :: Show b
  => Context -> Direction -> b -> IO b
groomedLog c d s = (logD c d . cs . groom) s >> return s

stdOutAndLog :: Context -> Direction -> Text -> IO ()
stdOutAndLog c d s =
  logD c d s >> ((putText . cs . show) d >> putText (s <> "\n"))

instance Show Direction where
  show From = "--->"
  show To   = "<---"
  show None = ""
