{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Betfair.API.ReadFromTChan
  (nonBlockingReadMarketIds
  ,nonBlockingReadFromTChan
  ,readMarketIdsFromTChan
  ,nonBlockingReadMarketIdsFromTChan)
  where

import Control.Concurrent.STM.TChan (TChan, readTChan, tryReadTChan)
import Control.Monad.RWS
import Control.Monad.STM            (atomically)
import Data.Maybe                   (isJust)

import Network.Betfair.API.CommonTypes
import Network.Betfair.API.Context
import Network.Betfair.API.StreamingState

nonBlockingReadFromTChan
  :: TChan a -> IO (Maybe a)
nonBlockingReadFromTChan chan =
  do mktId <- atomically $ tryReadTChan chan
     --   when (isJust mktId) $ putStrLn $ "Read value: " ++ show mktId
     return mktId

-- blocking read
readMarketIdsFromTChan
  :: TChan String -> IO [MarketId]
readMarketIdsFromTChan chan =
  do mktIds <- atomically $ readTChan chan
     putStrLn $ "Read value: " ++ show mktIds
     return (words mktIds)

nonBlockingReadMarketIdsFromTChan
  :: TChan String -> IO ([MarketId])
nonBlockingReadMarketIdsFromTChan chan =
  nonBlockingReadFromTChan chan >>= return . maybe [] words

nonBlockingReadMarketIds
  :: RWST Context () StreamingState IO ([MarketId])
nonBlockingReadMarketIds =
  ask >>= lift . nonBlockingReadMarketIdsFromTChan . cReadMarketIdsChannel