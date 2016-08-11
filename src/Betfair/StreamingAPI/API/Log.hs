{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Log
  (Log
  ,Direction(..)
  ,log
  ,logT
  ,groomedLog
  ,stdOutAndLog)
  where

import BasicPrelude
import Betfair.StreamingAPI.API.Context
import Control.Concurrent.STM.TChan
import Control.Monad.RWS
import Control.Monad.STM                (atomically)
import Data.Text
import Data.Text.IO                     hiding (putStr)
import Prelude                          hiding (concat, log, putStrLn)
import Text.Groom                       (groom)

type Log = ()

data Direction
  = From
  | To
  | None

logD :: TChan Text -> Direction -> Text -> IO ()
logD channel d s =
  (atomically . writeTChan channel . concat) [(pack . show) d,s,singleton '\n']

log :: TChan Text -> Text -> IO ()
log channel = atomically . writeTChan channel . flip append (singleton '\n')

logT
  :: Direction -> Text -> RWST Context () s IO ()
logT d s =
  do chan <- fmap cWriteLogChannel ask
     lift (logD chan d s)

groomedLog
  :: Show a
  => Direction -> a -> RWST Context () s IO a
groomedLog d s = (logT d . pack . groom $ s) >> return s

stdOutAndLog
  :: Direction -> Text -> RWST Context () s IO ()
stdOutAndLog d s = logT d s >> lift ((putStr . show) d >> putStrLn s)

instance Show Direction where
  show From = "--->"
  show To   = "<---"
  show None = ""
