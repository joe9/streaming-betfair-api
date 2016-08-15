{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Betfair.StreamingAPI.API.Clks
  (Clks(..))
  where

import           BasicPrelude            hiding (show)
--
data Clks =
  Clks { cClk :: Maybe Text
       , cInitialClk :: Maybe Text
       } deriving (Eq,Read,Show)
