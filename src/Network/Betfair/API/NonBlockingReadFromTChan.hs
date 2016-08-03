{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Betfair.API.NonBlockingReadFromTChan
  (nonBlockingReadFromTChan)
  where

import           Data.Maybe                   ( isJust)
import           Control.Monad.STM            (atomically)
import           Control.Concurrent.STM.TChan (TChan,
                                               tryReadTChan)
import           Control.Monad.RWS

import           Network.Betfair.API.CommonTypes

nonBlockingReadFromTChan :: TChan String -> IO (Maybe MarketId)
nonBlockingReadFromTChan chan = do
  mktId <- atomically $ tryReadTChan chan
  when (isJust mktId) $ putStrLn $ "Read value: " ++ show mktId
  return mktId
