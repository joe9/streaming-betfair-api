{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.MarketDefinition
  (MarketDefinition(..))
  where

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)

import Network.Betfair.Types.BettingType  (BettingType)
import Network.Betfair.Types.MarketStatus (MarketStatus)
import Network.Betfair.Types.RunnerDefinition (RunnerDefinition)

type DateString = Text

data MarketDefinition =
  MarketDefinition {venue                 :: Maybe Text
                   ,settledTime           :: Maybe DateString
                   ,timezone              :: Maybe Text
                   ,eachWayDivisor        :: Maybe Double
                   ,regulators            :: [String] -- The market regulators
                   ,marketType            :: Text
                   ,marketBaseRate        :: Maybe Double
                   ,numberOfWinners       :: Integer
                   ,countryCode           :: Maybe Text
                   ,inPlay                :: Bool
                   ,betDelay              :: Integer
                   ,bspMarket             :: Bool
                   ,bettingTypes          :: [BettingType]
                   ,numberOfActiveRunners :: Integer
                   ,eventId               :: Text
                   ,crossMatching         :: Bool
                   ,runnersVoidable       :: Bool
                   ,turnInPlayEnabled     :: Bool
                   ,suspendTime           :: DateString
                   ,discountAllowed       :: Bool
                   ,persistenceEnabled    :: Bool
                   ,runners               :: [RunnerDefinition]
                   ,version               :: Integer
                   ,eventTypeId           :: Text -- The Event Type the market is contained within
                   ,complete              :: Bool
                   ,openDate              :: DateString
                   ,marketTime            :: DateString
                   ,bspReconciled         :: Bool
                   ,status                :: MarketStatus}
  deriving (Eq,Read,Show)

-- deriveDefault ''MarketDefinition
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''MarketDefinition)
