{-# OPTIONS_GHC -Wall    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Betfair.Types.Order
  (Order(..))
  where

import Data.Aeson.TH   (Options (omitNothingFields), defaultOptions,
                        deriveJSON)
import Data.Default.TH (deriveDefault)

import Network.Betfair.Types.OrderStatus     (OrderStatus)
import Network.Betfair.Types.OrderType       (OrderType)
import Network.Betfair.Types.PersistenceType (PersistenceType)
import Network.Betfair.Types.Side            (Side)

type DateString = String

data Order =
  Order {side   :: Side -- Side - the side of the order
        ,sv     :: Maybe Double -- Size Voided - the amount of the order that has been voided
        ,pt     :: PersistenceType -- Persistence Type - whether the order will persist at in play or not (L = LAPSE, P = PERSIST, MOC = Market On Close)
        ,ot     :: OrderType -- Order Type - the type of the order (L = LIMIT, MOC = MARKET_ON_CLOSE, LOC = LIMIT_ON_CLOSE)
        ,p      :: Maybe Double -- Price - the original placed price of the order
        ,sc     :: Maybe Double -- Size Cancelled - the amount of the order that has been cancelled
        ,rc     :: Maybe String -- Regulator Code - the regulator of the order
        ,s      :: Maybe Double -- Size - the original placed size of the order
        ,pd     :: Maybe DateString -- Placed Date - the date the order was placed
        ,rac    :: Maybe String -- Regulator Auth Code - the auth code returned by the regulator
        ,md     :: Maybe Integer -- Matched Date - the date the order was matched (null if the order is not matched)
        ,sl     :: Maybe Double -- Size Lapsed - the amount of the order that has been lapsed
        ,avp    :: Maybe Double -- Average Price Matched - the average price the order was matched at (null if the order is not matched
        ,sm     :: Maybe Double -- Size Matched - the amount of the order that has been matched
        ,id     :: String -- Bet Id - the id of the order
        ,bsp    :: Maybe Double -- BSP Liability - the BSP liability of the order (null if the order is not a BSP order)
        ,status :: OrderStatus -- Status - the status of the order (E = EXECUTABLE, EC = EXECUTION_COMPLETE)
        ,sr     :: Maybe Double -- Size Remaining - the amount of the order that is remaining unmatched
        }
  deriving (Eq,Show)

deriveDefault ''Order
$(deriveJSON defaultOptions {omitNothingFields = True}
             ''Order)
