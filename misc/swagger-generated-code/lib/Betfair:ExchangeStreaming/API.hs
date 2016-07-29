{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, FlexibleInstances, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, DeriveTraversable, FlexibleContexts, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=302 #-}
module Betfair:ExchangeStreaming.API (
  -- * Client and Server
  ServerConfig(..),
  Betfair:ExchangeStreamingBackend,
  createBetfair:ExchangeStreamingClient,
  runBetfair:ExchangeStreamingServer,
  runBetfair:ExchangeStreamingClient,
  runBetfair:ExchangeStreamingClientWithManager,
  Betfair:ExchangeStreamingClient,
  -- ** Servant
  Betfair:ExchangeStreamingAPI,
  ) where

import Betfair:ExchangeStreaming.Types

import Data.Aeson (Value)
import Data.Coerce (coerce)
import Servant.API
import Servant (serve, ServantErr)
import Web.HttpApiData
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Text as T
import Data.Text (Text)
import Servant.Common.BaseUrl(BaseUrl(..))
import Servant.Client (ServantError, client, Scheme(Http))
import Data.Proxy (Proxy(..))
import Control.Monad.IO.Class
import Data.Function ((&))
import GHC.Exts (IsString(..))
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Monoid ((<>))
import Servant.API.Verbs (Verb, StdMethod(..))
import Control.Monad.Except (ExceptT)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Types.Method (methodOptions)

instance ReflectMethod 'OPTIONS where
  reflectMethod _ = methodOptions




-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either Text b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> key <> " in form data"
    Just value -> parseQueryParam value

-- | Servant type-level API, generated from the Swagger spec for Betfair:ExchangeStreaming.
type Betfair:ExchangeStreamingAPI
    =    "request" :> ReqBody '[JSON] AllRequestTypesExample :> Verb 'POST 200 '[JSON] AllResponseTypesExample -- 'requestPost' route

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig {
    configHost :: String,  -- ^ Hostname to serve on, e.g. "127.0.0.1"
    configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList { fromQueryList :: [a] }
  deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat = CommaSeparated -- ^ CSV format for multiple parameters.
                      | SpaceSeparated -- ^ Also called "SSV"
                      | TabSeparated -- ^ Also called "TSV"
                      | PipeSeparated -- ^ `value1|value2|value2`
                      | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
    parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
    parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
    parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
    parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
    parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
    toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
    toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
    toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
    toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
    toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Backend for Betfair:ExchangeStreaming.
-- The backend can be used both for the client and the server. The client generated from the Betfair:ExchangeStreaming Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createBetfair:ExchangeStreamingClient@). Alternatively, provided
-- a backend, the API can be served using @runBetfair:ExchangeStreamingServer@.
data Betfair:ExchangeStreamingBackend m = Betfair:ExchangeStreamingBackend {
    requestPost :: AllRequestTypesExample -> m AllResponseTypesExample{- ^ This is a socket protocol delimited by CRLF (not http) -}
  }

newtype Betfair:ExchangeStreamingClient a = Betfair:ExchangeStreamingClient { runClient :: Manager -> BaseUrl -> ExceptT ServantError IO a }
    deriving Functor

instance Applicative Betfair:ExchangeStreamingClient where
    pure x = Betfair:ExchangeStreamingClient (\_ _ -> pure x)
    (Betfair:ExchangeStreamingClient f) <*> (Betfair:ExchangeStreamingClient x) = Betfair:ExchangeStreamingClient (\manager url -> f manager url <*> x manager url)

instance Monad Betfair:ExchangeStreamingClient where
    (Betfair:ExchangeStreamingClient a) >>= f = Betfair:ExchangeStreamingClient (\manager url -> do
        value <- a manager url
        runClient (f value) manager url)

instance MonadIO Betfair:ExchangeStreamingClient where
    liftIO io = Betfair:ExchangeStreamingClient (\_ _ -> liftIO io)

createBetfair:ExchangeStreamingClient :: Betfair:ExchangeStreamingBackend Betfair:ExchangeStreamingClient
createBetfair:ExchangeStreamingClient = Betfair:ExchangeStreamingBackend{..}
  where
    ((coerce -> requestPost)) = client (Proxy :: Proxy Betfair:ExchangeStreamingAPI)

-- | Run requests in the Betfair:ExchangeStreamingClient monad.
runBetfair:ExchangeStreamingClient :: ServerConfig -> Betfair:ExchangeStreamingClient a -> ExceptT ServantError IO a
runBetfair:ExchangeStreamingClient clientConfig cl = do
  manager <- liftIO $ newManager defaultManagerSettings
  runBetfair:ExchangeStreamingClientWithManager manager clientConfig cl

-- | Run requests in the Betfair:ExchangeStreamingClient monad using a custom manager.
runBetfair:ExchangeStreamingClientWithManager :: Manager -> ServerConfig -> Betfair:ExchangeStreamingClient a -> ExceptT ServantError IO a
runBetfair:ExchangeStreamingClientWithManager manager clientConfig cl =
  runClient cl manager $ BaseUrl Http (configHost clientConfig) (configPort clientConfig) ""

-- | Run the Betfair:ExchangeStreaming server at the provided host and port.
runBetfair:ExchangeStreamingServer :: MonadIO m => ServerConfig -> Betfair:ExchangeStreamingBackend (ExceptT ServantErr IO)  -> m ()
runBetfair:ExchangeStreamingServer ServerConfig{..} backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy Betfair:ExchangeStreamingAPI) (serverFromBackend backend)

  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend Betfair:ExchangeStreamingBackend{..} =
      (coerce requestPost)
