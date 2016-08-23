{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Sample where

import BasicPrelude
import Data.Default
--
import           Betfair.BulkStreamingAPI

--
-- app key from betfair subscription
-- session token from the api
main :: IO ()
main = void (stream (undefined :: AppKey) (undefined :: SessionToken))

stream
  :: AppKey -> SessionToken -> IO StreamingState
stream a = fmap cState . startStreaming . initializeContext a
