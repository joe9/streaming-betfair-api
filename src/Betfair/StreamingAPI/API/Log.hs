{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
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

import qualified Data.Text                      as T
import           GHC.Show
import           Protolude                      hiding (empty, show)
import qualified Protolude
import           Text.PrettyPrint.GenericPretty (empty, space, string)
import qualified Text.PrettyPrint.GenericPretty as PP

import Text.PrettyPrint.GenericPretty (Pretty (..),
                                       displayPrettyPrefix, pretty)

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

instance Pretty Direction where
  pretty From = string "--->" PP.<> space
  pretty To   = string "<---" PP.<> space
  pretty None = empty

toLog :: Context -> Text -> IO ()
toLog = cLogger

toText
  :: Show a
  => Direction -> a -> Text
toText d t = Protolude.show d <> T.singleton (' ' :: Char) <> Protolude.show t

stdOutAndLog :: Context -> Direction -> Text -> IO ()
stdOutAndLog c d s =
  let output = displayPrettyPrefix d s
  in toLog c output >> putText s

tracePPLog
  :: Pretty a
  => Context -> Direction -> a -> IO a
tracePPLog c d s = (toLog c . displayPrettyPrefix d) s >> return s

traceLog
  :: Show a
  => Context -> Direction -> a -> IO a
traceLog c d s = (toLog c . toText d) s >> return s
