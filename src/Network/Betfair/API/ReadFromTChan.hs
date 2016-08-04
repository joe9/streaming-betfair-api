{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Betfair.API.ReadFromTChan
  (nonBlockingReadFromTChan, readMarketIdsFromTChan, nonBlockingReadMarketIdsFromTChan)
  where

import           Data.Maybe                   ( isJust)
import           Control.Monad.STM            (atomically)
import           Control.Concurrent.STM.TChan (TChan,
                                               tryReadTChan, readTChan)

import           Network.Betfair.API.CommonTypes

nonBlockingReadFromTChan :: TChan a -> IO (Maybe a)
nonBlockingReadFromTChan chan = do
  mktId <- atomically $ tryReadTChan chan
--   when (isJust mktId) $ putStrLn $ "Read value: " ++ show mktId
  return mktId

-- blocking read
readMarketIdsFromTChan :: TChan String -> IO [MarketId]
readMarketIdsFromTChan chan = do
  mktIds <- atomically $ readTChan chan
  putStrLn $ "Read value: " ++ show mktIds
  return (words mktIds)

nonBlockingReadMarketIdsFromTChan :: TChan String -> IO ([MarketId])
nonBlockingReadMarketIdsFromTChan chan =
  nonBlockingReadFromTChan chan >>= return . maybe [] words

