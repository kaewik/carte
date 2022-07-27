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
  -- ** Authentication
  , CarteAuth(..)
  , clientAuth
  , Protected
  ) where

import           Carte.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.ByteString                    (ByteString)
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
import           Network.Wai                        (Middleware, Request, requestHeaders)
import qualified Network.Wai.Handler.Warp           as Warp
import           Network.Wai.Middleware.HttpAuth    (extractBearerAuth)
import           Servant                            (ServerError, serveWithContextT, throwError)
import           Servant.API                        hiding (addHeader)
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.API.Experimental.Auth      (AuthProtect)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application, Context ((:.), EmptyContext))
import           Servant.Server.Experimental.Auth   (AuthHandler, AuthServerData, mkAuthHandler)
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
    =    "auth" :> Header "X-Authorization" Text :> Verb 'GET 200 '[JSON] GetAuth200Response -- 'getAuth' route
    :<|> Protected :> "health" :> Verb 'GET 200 '[JSON] NoContent -- 'getHealth' route
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
data CarteBackend a m = CarteBackend
  { getAuth :: Maybe Text -> m GetAuth200Response{- ^ check username and password and provide a JSON Web token (JWT) with information about permissions -}
  , getHealth :: a -> m NoContent{- ^ health check -}
  }

-- | Authentication settings for Carte.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data CarteAuth = CarteAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
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

createCarteClient :: CarteBackend AuthClient CarteClient
createCarteClient = CarteBackend{..}
  where
    ((coerce -> getAuth) :<|>
     (coerce -> getHealth) :<|>
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
  => Config -> CarteAuth -> CarteBackend AuthServer (ExceptT ServerError IO) -> m ()
runCarteServer config auth backend = runCarteMiddlewareServer config requestMiddlewareId auth backend

-- | Run the Carte server at the provided host and port.
runCarteMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> CarteAuth -> CarteBackend AuthServer (ExceptT ServerError IO) -> m ()
runCarteMiddlewareServer Config{..} middleware auth backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationCarte auth backend

-- | Plain "Network.Wai" Application for the Carte server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationCarte :: CarteAuth -> CarteBackend AuthServer (ExceptT ServerError IO) -> Application
serverWaiApplicationCarte auth backend = serveWithContextT (Proxy :: Proxy CarteAPI) context id (serverFromBackend backend)
  where
    context = serverContext auth
    serverFromBackend CarteBackend{..} =
      (coerce getAuth :<|>
       coerce getHealth :<|>
       serveDirectoryFileServer "static")

-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: CarteAuth -> AuthHandler Request AuthServer
authHandler CarteAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Authorization" (requestHeaders req) of
      Just header -> case extractBearerAuth header of
        Just key -> lookupUser key
        Nothing -> throwError (authError req)
      Nothing -> throwError (authError req)

type Protected = AuthProtect "bearer"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest ("Bearer " <> key) (addHeader "Authorization")

serverContext :: CarteAuth -> Context (AuthHandler Request AuthServer ': '[])
serverContext auth = authHandler auth :. EmptyContext
