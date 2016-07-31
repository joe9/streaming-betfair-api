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

type DateString = String

data MarketDefinition =
  MarketDefinition {venue                 :: Maybe String
                   ,settledTime           :: Maybe DateString
                   ,timezone              :: Maybe String
                   ,eachWayDivisor        :: Maybe Double
                   ,regulators            :: [String] -- The market regulators
                   ,marketType            :: String
                   ,marketBaseRate        :: Maybe Double
                   ,numberOfWinners       :: Integer
                   ,countryCode           :: Maybe String
                   ,inPlay                :: Bool
                   ,betDelay              :: Integer
                   ,bspMarket             :: Bool
                   ,bettingTypes          :: [BettingType]
                   ,numberOfActiveRunners :: Integer
                   ,eventId               :: String
                   ,crossMatching         :: Bool
                   ,runnersVoidable       :: Bool
                   ,turnInPlayEnabled     :: Bool
                   ,suspendTime           :: DateString
                   ,discountAllowed       :: Bool
                   ,persistenceEnabled    :: Bool
                   ,runners               :: [RunnerDefinition]
                   ,version               :: Integer
                   ,eventTypeId           :: String -- The Event Type the market is contained within
                   ,complete              :: Bool
                   ,openDate              :: DateString
                   ,marketTime            :: DateString
                   ,bspReconciled         :: Bool
                   ,status                :: MarketStatus}
  deriving (Eq,Show)

-- deriveDefault ''MarketDefinition
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''MarketDefinition)
