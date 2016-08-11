{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.ReadFromTChan
  (nonBlockingReadMarketIds
  ,nonBlockingReadFromTChan
  ,readMarketIdsFromTChan
  ,nonBlockingReadMarketIdsFromTChan)
  where

import BasicPrelude
import Betfair.StreamingAPI.API.CommonTypes
import Betfair.StreamingAPI.API.Context
import Betfair.StreamingAPI.API.StreamingState
import Control.Concurrent.STM.TChan            (TChan, readTChan,
                                                tryReadTChan)
import Control.Monad.RWS
import Control.Monad.STM                       (atomically)
import Data.Text
import Prelude                                 hiding (words)

nonBlockingReadFromTChan
  :: TChan a -> IO (Maybe a)
nonBlockingReadFromTChan chan =
  do mktId <- atomically $ tryReadTChan chan
     --   when (isJust mktId) $ putStrLn $ "Read value: " ++ show mktId
     return mktId

-- blocking read
readMarketIdsFromTChan
  :: TChan Text -> IO [MarketId]
readMarketIdsFromTChan chan =
  do mktIds <- atomically $ readTChan chan
     putStrLn $ "Read value: " ++ show mktIds
     return (words mktIds)

nonBlockingReadMarketIdsFromTChan
  :: TChan Text -> IO ([MarketId])
nonBlockingReadMarketIdsFromTChan chan =
  nonBlockingReadFromTChan chan >>= return . maybe [] words

nonBlockingReadMarketIds
  :: RWST Context () StreamingState IO ([MarketId])
nonBlockingReadMarketIds =
  ask >>= lift . nonBlockingReadMarketIdsFromTChan . cReadMarketIdsChannel
