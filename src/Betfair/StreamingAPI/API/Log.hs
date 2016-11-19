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

import GHC.Show
import Protolude               hiding (show,log)
import qualified Protolude
import Data.Text.Lazy.Builder
import qualified Data.Text as T
import Data.Aeson.Encode.Pretty
import Data.Aeson

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
toLog c = cLogger c

dirPrefix :: Direction -> Builder
dirPrefix = fromText . Protolude.show

ppText :: ToJSON a => Direction -> a -> Text
ppText d = toStrict . toLazyText . mappend (dirPrefix d <> singleton ' ') . encodePrettyToTextBuilder

toText :: Show a => Direction -> a -> Text
toText d t = Protolude.show d <> T.singleton (' ' :: Char) <> Protolude.show t

stdOutAndLog :: Context -> Direction -> Text -> IO ()
stdOutAndLog c d s = let output = ppText d s
                     in toLog c output >> putText (s <> "\n")

tracePPLog
  :: ToJSON a
  => Context -> Direction -> a -> IO a
tracePPLog c d s = (toLog c . ppText d) s >> return s

traceLog :: Show a => Context -> Direction -> a -> IO a
traceLog c d s = (toLog c . toText d) s >> return s
