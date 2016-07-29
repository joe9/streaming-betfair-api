{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Betfair:ExchangeStreaming.Types (
    AllRequestTypesExample (..),
    AllResponseTypesExample (..),
    AuthenticationMessage (..),
    ConnectionMessage (..),
    HeartbeatMessage (..),
    MarketChange (..),
    MarketChangeMessage (..),
    MarketDataFilter (..),
    MarketDefinition (..),
    MarketFilter (..),
    MarketSubscriptionMessage (..),
    Order (..),
    OrderChangeMessage (..),
    OrderFilter (..),
    OrderMarketChange (..),
    OrderRunnerChange (..),
    OrderSubscriptionMessage (..),
    RequestMessage (..),
    ResponseMessage (..),
    RunnerChange (..),
    RunnerDefinition (..),
    StatusMessage (..),
    ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data AllRequestTypesExample = AllRequestTypesExample
    { allRequestTypesExampleOpTypes :: Text -- ^ 
    , allRequestTypesExampleHeartbeat :: HeartbeatMessage -- ^ 
    , allRequestTypesExampleOrderSubscriptionMessage :: OrderSubscriptionMessage -- ^ 
    , allRequestTypesExampleMarketSubscription :: MarketSubscriptionMessage -- ^ 
    , allRequestTypesExampleAuthentication :: AuthenticationMessage -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON AllRequestTypesExample where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "allRequestTypesExample")
instance ToJSON AllRequestTypesExample where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "allRequestTypesExample")

-- | 
data AllResponseTypesExample = AllResponseTypesExample
    { allResponseTypesExampleOpTypes :: Text -- ^ 
    , allResponseTypesExampleMarketChangeMessage :: MarketChangeMessage -- ^ 
    , allResponseTypesExampleConnection :: ConnectionMessage -- ^ 
    , allResponseTypesExampleOrderChangeMessage :: OrderChangeMessage -- ^ 
    , allResponseTypesExampleStatus :: StatusMessage -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON AllResponseTypesExample where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "allResponseTypesExample")
instance ToJSON AllResponseTypesExample where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "allResponseTypesExample")

-- | 
newtype AuthenticationMessage = AuthenticationMessage { unAuthenticationMessage :: RequestMessage }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
newtype ConnectionMessage = ConnectionMessage { unConnectionMessage :: ResponseMessage }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
newtype HeartbeatMessage = HeartbeatMessage { unHeartbeatMessage :: RequestMessage }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
data MarketChange = MarketChange
    { marketChangeRc :: [RunnerChange] -- ^ Runner Changes - a list of changes to runners (or null if un-changed)
    , marketChangeImg :: Bool -- ^ Image - replace existing prices / data with the data supplied: it is not a delta (or null if delta)
    , marketChangeTv :: Double -- ^ The total amount matched across the market. This value is truncated at 2dp (or null if un-changed)
    , marketChangeCon :: Bool -- ^ Conflated - have more than a single change been combined (or null if not conflated)
    , marketChangeMarketDefinition :: MarketDefinition -- ^ Market Definition - the definition of the market (or null if un-changed)
    , marketChangeId :: Text -- ^ Market Id - the id of the market
    } deriving (Show, Eq, Generic)

instance FromJSON MarketChange where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "marketChange")
instance ToJSON MarketChange where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "marketChange")

-- | 
newtype MarketChangeMessage = MarketChangeMessage { unMarketChangeMessage :: ResponseMessage }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
data MarketDataFilter = MarketDataFilter
    { marketDataFilterLadderLevels :: Int -- ^ 
    , marketDataFilterFields :: [Text] -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON MarketDataFilter where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "marketDataFilter")
instance ToJSON MarketDataFilter where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "marketDataFilter")

-- | 
data MarketDefinition = MarketDefinition
    { marketDefinitionVenue :: Text -- ^ 
    , marketDefinitionSettledTime :: Integer -- ^ 
    , marketDefinitionTimezone :: Text -- ^ 
    , marketDefinitionEachWayDivisor :: Double -- ^ 
    , marketDefinitionRegulators :: [Text] -- ^ The market regulators.
    , marketDefinitionMarketType :: Text -- ^ 
    , marketDefinitionMarketBaseRate :: Double -- ^ 
    , marketDefinitionNumberOfWinners :: Int -- ^ 
    , marketDefinitionCountryCode :: Text -- ^ 
    , marketDefinitionInPlay :: Bool -- ^ 
    , marketDefinitionBetDelay :: Int -- ^ 
    , marketDefinitionBspMarket :: Bool -- ^ 
    , marketDefinitionBettingType :: Text -- ^ 
    , marketDefinitionNumberOfActiveRunners :: Int -- ^ 
    , marketDefinitionEventId :: Text -- ^ 
    , marketDefinitionCrossMatching :: Bool -- ^ 
    , marketDefinitionRunnersVoidable :: Bool -- ^ 
    , marketDefinitionTurnInPlayEnabled :: Bool -- ^ 
    , marketDefinitionSuspendTime :: Integer -- ^ 
    , marketDefinitionDiscountAllowed :: Bool -- ^ 
    , marketDefinitionPersistenceEnabled :: Bool -- ^ 
    , marketDefinitionRunners :: [RunnerDefinition] -- ^ 
    , marketDefinitionVersion :: Integer -- ^ 
    , marketDefinitionEventTypeId :: Text -- ^ The Event Type the market is contained within.
    , marketDefinitionComplete :: Bool -- ^ 
    , marketDefinitionOpenDate :: Integer -- ^ 
    , marketDefinitionMarketTime :: Integer -- ^ 
    , marketDefinitionBspReconciled :: Bool -- ^ 
    , marketDefinitionStatus :: Text -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON MarketDefinition where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "marketDefinition")
instance ToJSON MarketDefinition where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "marketDefinition")

-- | 
data MarketFilter = MarketFilter
    { marketFilterCountryCodes :: [Text] -- ^ 
    , marketFilterBettingTypes :: [Text] -- ^ 
    , marketFilterTurnInPlayEnabled :: Bool -- ^ 
    , marketFilterMarketTypes :: [Text] -- ^ 
    , marketFilterVenues :: [Text] -- ^ 
    , marketFilterMarketIds :: [Text] -- ^ 
    , marketFilterEventTypeIds :: [Text] -- ^ 
    , marketFilterEventIds :: [Text] -- ^ 
    , marketFilterBspMarket :: Bool -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON MarketFilter where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "marketFilter")
instance ToJSON MarketFilter where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "marketFilter")

-- | 
newtype MarketSubscriptionMessage = MarketSubscriptionMessage { unMarketSubscriptionMessage :: RequestMessage }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
data Order = Order
    { orderSide :: Text -- ^ Side - the side of the order
    , orderSv :: Double -- ^ Size Voided - the amount of the order that has been voided
    , orderPt :: Text -- ^ Persistence Type - whether the order will persist at in play or not (L = LAPSE, P = PERSIST, MOC = Market On Close)
    , orderOt :: Text -- ^ Order Type - the type of the order (L = LIMIT, MOC = MARKET_ON_CLOSE, LOC = LIMIT_ON_CLOSE)
    , orderP :: Double -- ^ Price - the original placed price of the order
    , orderSc :: Double -- ^ Size Cancelled - the amount of the order that has been cancelled
    , orderRc :: Text -- ^ Regulator Code - the regulator of the order
    , orderS :: Double -- ^ Size - the original placed size of the order
    , orderPd :: Integer -- ^ Placed Date - the date the order was placed
    , orderRac :: Text -- ^ Regulator Auth Code - the auth code returned by the regulator
    , orderMd :: Integer -- ^ Matched Date - the date the order was matched (null if the order is not matched)
    , orderSl :: Double -- ^ Size Lapsed - the amount of the order that has been lapsed
    , orderAvp :: Double -- ^ Average Price Matched - the average price the order was matched at (null if the order is not matched
    , orderSm :: Double -- ^ Size Matched - the amount of the order that has been matched
    , orderId :: Text -- ^ Bet Id - the id of the order
    , orderBsp :: Double -- ^ BSP Liability - the BSP liability of the order (null if the order is not a BSP order)
    , orderStatus :: Text -- ^ Status - the status of the order (E = EXECUTABLE, EC = EXECUTION_COMPLETE)
    , orderSr :: Double -- ^ Size Remaining - the amount of the order that is remaining unmatched
    } deriving (Show, Eq, Generic)

instance FromJSON Order where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "order")
instance ToJSON Order where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "order")

-- | 
newtype OrderChangeMessage = OrderChangeMessage { unOrderChangeMessage :: ResponseMessage }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
data OrderFilter = OrderFilter
    { orderFilterAccountIds :: [Integer] -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON OrderFilter where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "orderFilter")
instance ToJSON OrderFilter where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "orderFilter")

-- | 
data OrderMarketChange = OrderMarketChange
    { orderMarketChangeAccountId :: Integer -- ^ 
    , orderMarketChangeOrc :: [OrderRunnerChange] -- ^ Order Changes - a list of changes to orders on a selection
    , orderMarketChangeClosed :: Bool -- ^ 
    , orderMarketChangeId :: Text -- ^ Market Id - the id of the market the order is on
    } deriving (Show, Eq, Generic)

instance FromJSON OrderMarketChange where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "orderMarketChange")
instance ToJSON OrderMarketChange where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "orderMarketChange")

-- | 
data OrderRunnerChange = OrderRunnerChange
    { orderRunnerChangeMb :: [[Double]] -- ^ Matched Backs - matched amounts by distinct matched price on the Back side for this runner (selection)
    , orderRunnerChangeUo :: [Order] -- ^ Unmatched Orders - orders on this runner (selection) that are not fully matched
    , orderRunnerChangeId :: Integer -- ^ Selection Id - the id of the runner (selection)
    , orderRunnerChangeHc :: Double -- ^ Handicap - the handicap of the runner (selection) (null if not applicable)
    , orderRunnerChangeFullImage :: Bool -- ^ 
    , orderRunnerChangeMl :: [[Double]] -- ^ Matched Lays - matched amounts by distinct matched price on the Lay side for this runner (selection)
    } deriving (Show, Eq, Generic)

instance FromJSON OrderRunnerChange where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "orderRunnerChange")
instance ToJSON OrderRunnerChange where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "orderRunnerChange")

-- | 
newtype OrderSubscriptionMessage = OrderSubscriptionMessage { unOrderSubscriptionMessage :: RequestMessage }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
data RequestMessage = RequestMessage
    { requestMessageOp :: Text -- ^ The operation type
    , requestMessageId :: Int -- ^ Client generated unique id to link request with response (like json rpc)
    } deriving (Show, Eq, Generic)

instance FromJSON RequestMessage where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "requestMessage")
instance ToJSON RequestMessage where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "requestMessage")

-- | 
data ResponseMessage = ResponseMessage
    { responseMessageOp :: Text -- ^ The operation type
    , responseMessageId :: Int -- ^ Client generated unique id to link request with response (like json rpc)
    } deriving (Show, Eq, Generic)

instance FromJSON ResponseMessage where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "responseMessage")
instance ToJSON ResponseMessage where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "responseMessage")

-- | 
data RunnerChange = RunnerChange
    { runnerChangeTv :: Double -- ^ The total amount matched. This value is truncated at 2dp.
    , runnerChangeBatb :: [[Double]] -- ^ Best Available To Back - LevelPriceVol triple delta of price changes, keyed by level (0 vol is remove)
    , runnerChangeSpb :: [[Double]] -- ^ Starting Price Back - PriceVol tuple delta of price changes (0 vol is remove)
    , runnerChangeBdatl :: [[Double]] -- ^ Best Display Available To Lay (includes virtual prices)- LevelPriceVol triple delta of price changes, keyed by level (0 vol is remove)
    , runnerChangeTrd :: [[Double]] -- ^ Traded - PriceVol tuple delta of price changes (0 vol is remove)
    , runnerChangeSpf :: Double -- ^ Starting Price Far - The far starting price (or null if un-changed)
    , runnerChangeLtp :: Double -- ^ Last Traded Price - The last traded price (or null if un-changed)
    , runnerChangeAtb :: [[Double]] -- ^ Available To Back - PriceVol tuple delta of price changes (0 vol is remove)
    , runnerChangeSpl :: [[Double]] -- ^ Starting Price Lay - PriceVol tuple delta of price changes (0 vol is remove)
    , runnerChangeSpn :: Double -- ^ Starting Price Near - The far starting price (or null if un-changed)
    , runnerChangeAtl :: [[Double]] -- ^ Available To Lay - PriceVol tuple delta of price changes (0 vol is remove)
    , runnerChangeBatl :: [[Double]] -- ^ Best Available To Lay - LevelPriceVol triple delta of price changes, keyed by level (0 vol is remove)
    , runnerChangeId :: Integer -- ^ Selection Id - the id of the runner (selection)
    , runnerChangeHc :: Double -- ^ Handicap - the handicap of the runner (selection) (null if not applicable)
    , runnerChangeBdatb :: [[Double]] -- ^ Best Display Available To Back (includes virtual prices)- LevelPriceVol triple delta of price changes, keyed by level (0 vol is remove)
    } deriving (Show, Eq, Generic)

instance FromJSON RunnerChange where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "runnerChange")
instance ToJSON RunnerChange where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "runnerChange")

-- | 
data RunnerDefinition = RunnerDefinition
    { runnerDefinitionSortPriority :: Int -- ^ 
    , runnerDefinitionRemovalDate :: Integer -- ^ 
    , runnerDefinitionId :: Integer -- ^ Selection Id - the id of the runner (selection)
    , runnerDefinitionHc :: Double -- ^ Handicap - the handicap of the runner (selection) (null if not applicable)
    , runnerDefinitionAdjustmentFactor :: Double -- ^ 
    , runnerDefinitionBsp :: Double -- ^ 
    , runnerDefinitionStatus :: Text -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON RunnerDefinition where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "runnerDefinition")
instance ToJSON RunnerDefinition where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "runnerDefinition")

-- | 
newtype StatusMessage = StatusMessage { unStatusMessage :: ResponseMessage }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars = [("@", "'At"), ("!", "'Exclamation"), ("#", "'Hash"), ("$", "'Dollar"), ("%", "'Percent"), ("&", "'Ampersand"), ("*", "'Star"), ("+", "'Plus"), ("-", "'Dash"), (":", "'Colon"), ("|", "'Pipe"), ("<", "'LessThan"), ("=", "'Equal"), ("^", "'Caret"), (">", "'GreaterThan")]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer = if forParsing then flip T.replace else T.replace


