{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module Carte.API
  ( -- * Client and Server
    Config(..)
  , CarteBackend(..)
  , createCarteClient
  , runCarteServer
  , runCarteMiddlewareServer
  , runCarteClient
  , runCarteClientWithManager
  , callCarte
  , CarteClient
  , CarteClientError(..)
  -- ** Servant
  , CarteAPI
  -- ** Plain WAI Application
  , serverWaiApplicationCarte
  ) where

import           Carte.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serveWithContextT)
import           Servant.API                        hiding (addHeader)
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application, Context (EmptyContext))
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
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


-- | Servant type-level API, generated from the OpenAPI spec for Carte.
type CarteAPI
    =    "health" :> Verb 'GET 200 '[JSON] NoContent -- 'healthGet' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype CarteClientError = CarteClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for Carte.
-- The backend can be used both for the client and the server. The client generated from the Carte OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createCarteClient@). Alternatively, provided
-- a backend, the API can be served using @runCarteMiddlewareServer@.
data CarteBackend m = CarteBackend
  { healthGet :: m NoContent{- ^ For health check purposes -}
  }


newtype CarteClient a = CarteClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative CarteClient where
  pure x = CarteClient (\_ -> pure x)
  (CarteClient f) <*> (CarteClient x) =
    CarteClient (\env -> f env <*> x env)

instance Monad CarteClient where
  (CarteClient a) >>= f =
    CarteClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO CarteClient where
  liftIO io = CarteClient (\_ -> liftIO io)

createCarteClient :: CarteBackend CarteClient
createCarteClient = CarteBackend{..}
  where
    ((coerce -> healthGet) :<|>
     _) = client (Proxy :: Proxy CarteAPI)

-- | Run requests in the CarteClient monad.
runCarteClient :: Config -> CarteClient a -> ExceptT ClientError IO a
runCarteClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runCarteClientWithManager manager clientConfig cl

-- | Run requests in the CarteClient monad using a custom manager.
runCarteClientWithManager :: Manager -> Config -> CarteClient a -> ExceptT ClientError IO a
runCarteClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a CarteClientError
callCarte
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> CarteClient a -> m a
callCarte env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (CarteClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the Carte server at the provided host and port.
runCarteServer
  :: (MonadIO m, MonadThrow m)
  => Config -> CarteBackend (ExceptT ServerError IO) -> m ()
runCarteServer config backend = runCarteMiddlewareServer config requestMiddlewareId backend

-- | Run the Carte server at the provided host and port.
runCarteMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> CarteBackend (ExceptT ServerError IO) -> m ()
runCarteMiddlewareServer Config{..} middleware backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationCarte backend

-- | Plain "Network.Wai" Application for the Carte server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationCarte :: CarteBackend (ExceptT ServerError IO) -> Application
serverWaiApplicationCarte backend = serveWithContextT (Proxy :: Proxy CarteAPI) context id (serverFromBackend backend)
  where
    context = serverContext
    serverFromBackend CarteBackend{..} =
      (coerce healthGet :<|>
       serveDirectoryFileServer "static")


serverContext :: Context ('[])
serverContext = EmptyContext
