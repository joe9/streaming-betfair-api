{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Log
  (Log
  ,Direction(..)
  ,toLog
  ,logD
  ,groomedLog
  ,stdOutAndLog)
  where

import BasicPrelude            hiding (show)
import Data.String.Conversions
import GHC.Show
import Text.Groom              (groom)
--
import Betfair.StreamingAPI.API.Context

type Log = Text

data Direction
  = From
  | To
  | None

logD :: Context a -> Direction -> Text -> IO ()
logD c d s = toLog c ((cs . show) d <> s)

toLog :: Context a -> Text -> IO ()
toLog c = cLogger c

groomedLog
  :: Show b
  => Context a -> Direction -> b -> IO b
groomedLog c d s = (logD c d . cs . groom) s >> return s

stdOutAndLog
  :: Context a -> Direction -> Text -> IO ()
stdOutAndLog c d s = logD c d s >> ((putStr . cs . show) d >> putStrLn s)

instance Show Direction where
  show From = "--->"
  show To   = "<---"
  show None = ""
