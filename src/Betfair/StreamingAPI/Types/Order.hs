{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Betfair.StreamingAPI.Types.Order
  ( Order(..)
  ) where

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Protolude

--
import Betfair.StreamingAPI.Types.OrderStatus     (OrderStatus)
import Betfair.StreamingAPI.Types.OrderType       (OrderType)
import Betfair.StreamingAPI.Types.PersistenceType (PersistenceType)
import Betfair.StreamingAPI.Types.Side            (Side)

type DateString = Text

data Order = Order
  { side   :: Side -- Side - the side of the order
  , sv     :: Maybe Double -- Size Voided - the amount of the order that has been voided
  , pt     :: PersistenceType -- Persistence Type - whether the order will persist at in play or not (L = LAPSE, P = PERSIST, MOC = Market On Close)
  , ot     :: OrderType -- Order Type - the type of the order (L = LIMIT, MOC = MARKET_ON_CLOSE, LOC = LIMIT_ON_CLOSE)
  , p      :: Maybe Double -- Price - the original placed price of the order
  , sc     :: Maybe Double -- Size Cancelled - the amount of the order that has been cancelled
  , rc     :: Maybe Text -- Regulator Code - the regulator of the order
  , s      :: Maybe Double -- Size - the original placed size of the order
  , pd     :: Maybe DateString -- Placed Date - the date the order was placed
  , rac    :: Maybe Text -- Regulator Auth Code - the auth code returned by the regulator
  , md     :: Maybe Integer -- Matched Date - the date the order was matched (null if the order is not matched)
  , sl     :: Maybe Double -- Size Lapsed - the amount of the order that has been lapsed
  , avp    :: Maybe Double -- Average Price Matched - the average price the order was matched at (null if the order is not matched
  , sm     :: Maybe Double -- Size Matched - the amount of the order that has been matched
  , id     :: Text -- Bet Id - the id of the order
  , bsp    :: Maybe Double -- BSP Liability - the BSP liability of the order (null if the order is not a BSP order)
  , status :: OrderStatus -- Status - the status of the order (E = EXECUTABLE, EC = EXECUTION_COMPLETE)
  , sr     :: Maybe Double -- Size Remaining - the amount of the order that is remaining unmatched
  } deriving (Eq, Read, Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''Order)
