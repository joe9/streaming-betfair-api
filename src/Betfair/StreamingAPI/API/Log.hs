{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Log
  ( Log
  , Direction(..)
  , toLog
  , tracePPLog
  , traceLog
  , stdOutAndLog
  ) where

import Text.PrettyPrint.GenericPretty
import qualified Data.Text                as T
import           Data.Text.Lazy (toStrict)
import           GHC.Show
import           Protolude                hiding (show)
import qualified Protolude

import Betfair.StreamingAPI.API.Context

type Log = Text

data Direction
  = From
  | To
  | None

instance Show Direction where
  show From = "--->"
  show To   = "<---"
  show None = ""

toLog :: Context -> Text -> IO ()
toLog = cLogger

ppText
  :: Pretty a
  => Direction -> a -> Text
ppText d =
  mappend (Protolude.show d <> T.singleton ' ') . toStrict . displayPretty

toText
  :: Show a
  => Direction -> a -> Text
toText d t = Protolude.show d <> T.singleton (' ' :: Char) <> Protolude.show t

stdOutAndLog :: Context -> Direction -> Text -> IO ()
stdOutAndLog c d s =
  let output = ppText d s
  in toLog c output >> putText s

tracePPLog
  :: Pretty a
  => Context -> Direction -> a -> IO a
tracePPLog c d s = (toLog c . ppText d) s >> return s

traceLog
  :: Show a
  => Context -> Direction -> a -> IO a
traceLog c d s = (toLog c . toText d) s >> return s
