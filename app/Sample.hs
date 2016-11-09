{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import Data.Time

import Betfair.BulkStreamingAPI

-- app key from betfair subscription
-- session token from the api
main :: IO ()
main = void (stream (undefined :: AppKey) (undefined :: SessionToken))

stream
  :: AppKey -> SessionToken -> IO StreamingState
stream a t =
  getCurrentTime >>= fmap cState . startStreaming . initializeContext a t
