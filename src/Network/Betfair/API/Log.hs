{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.Betfair.API.Log
  (Log
  ,Direction(..)
  ,log
  ,logT
  ,groomedLog
  ,stdOutAndLog)
  where

import Control.Concurrent.STM.TChan
import Control.Monad.RWS
import Control.Monad.STM            (atomically)
import Prelude                      hiding (log)
import Text.Groom                   (groom)

import Network.Betfair.API.Context

type Log = ()

data Direction
  = From
  | To
  | None

logD
  :: TChan String -> Direction -> String -> IO ()
logD channel d s = (atomically . writeTChan channel) (show d ++ s ++ "\n")

log
  :: TChan String -> String -> IO ()
log channel s = (atomically . writeTChan channel) (s ++ "\n")

logT
  :: Direction -> String -> RWST Context () s IO ()
logT d s =
  do chan <- fmap cWriteLogChannel ask
     lift (logD chan d s)

groomedLog
  :: Show a
  => Direction -> a -> RWST Context () s IO a
groomedLog d s = (logT d . groom $ s) >> return s

stdOutAndLog
  :: Direction -> String -> RWST Context () s IO ()
stdOutAndLog d s = logT d s >> lift ((putStr . show) d >> putStrLn s)

instance Show Direction where
  show From = "--->"
  show To   = "<---"
  show None = ""
