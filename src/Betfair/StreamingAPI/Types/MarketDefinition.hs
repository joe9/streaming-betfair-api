{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Betfair.StreamingAPI.Types.MarketDefinition
  ( MarketDefinition(..)
  ) where

import Data.Aeson.TH                  (Options (omitNothingFields),
                                       defaultOptions, deriveJSON)
import Protolude

import Text.PrettyPrint.GenericPretty

import Betfair.StreamingAPI.Types.BettingType           (BettingType)
import Betfair.StreamingAPI.Types.KeyLineDefinition     (KeyLineDefinition)
import Betfair.StreamingAPI.Types.MarketStatus          (MarketStatus)
import Betfair.StreamingAPI.Types.PriceLadderDefinition (PriceLadderDefinition)
import Betfair.StreamingAPI.Types.RunnerDefinition      (RunnerDefinition)

type DateString = Text

data MarketDefinition = MarketDefinition
  { venue                 :: Maybe Text
  , settledTime           :: Maybe DateString
  , timezone              :: Maybe Text
  , eachWayDivisor        :: Maybe Double
  , regulators            :: [Text] -- The market regulators
  , marketType            :: Text
  , marketBaseRate        :: Maybe Double
  , numberOfWinners       :: Integer
  , countryCode           :: Maybe Text
  , lineMaxUnit           :: Maybe Integer
  , inPlay                :: Maybe Bool
  , betDelay              :: Integer
  , bspMarket             :: Bool
  , bettingTypes          :: Maybe [BettingType]
  , numberOfActiveRunners :: Integer
  , lineMinUnit           :: Maybe Integer
  , eventId               :: Text
  , crossMatching         :: Bool
  , runnersVoidable       :: Bool
  , turnInPlayEnabled     :: Bool
  , suspendTime           :: DateString
  , discountAllowed       :: Bool
  , persistenceEnabled    :: Bool
  , runners               :: [RunnerDefinition]
  , version               :: Integer
  , eventTypeId           :: Text -- The Event Type the market is contained within
  , complete              :: Bool
  , openDate              :: DateString
  , marketTime            :: DateString
  , bspReconciled         :: Bool
  , lineInterval          :: Maybe Integer
  , status                :: MarketStatus
  , priceLadderDefinition :: Maybe PriceLadderDefinition
  , keyLineDefinition     :: Maybe KeyLineDefinition
  } deriving (Eq, Read, Show, Generic, Pretty)

$(deriveJSON defaultOptions {omitNothingFields = True} ''MarketDefinition)
