{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Handlers (carteBackend, Config (Config), auth, runCarteMiddlewareServer) where

import Carte.API (CarteAuth (CarteAuth, authError, lookupUser), CarteBackend (CarteBackend, getAuth, getHealth), Config (Config), Protected, runCarteMiddlewareServer)
import Carte.Types (GetAuth200Response (GetAuth200Response, getAuth200ResponseJwt))
import Control.Monad.Except (ExceptT, withExceptT)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), maybeToExceptT)
import qualified Data.ByteString.Lazy as LBS
import Data.Password.Bcrypt (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash), checkPassword, mkPassword)
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.Encoding.Base64 (decodeBase64, isBase64)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.Persist (Entity (entityVal), getBy)
import Database.Persist.Sqlite (runSqlite)
import Entities (Unique (UniqueUsername), User (userEmail, userPassword))
import Servant (NoContent (NoContent), ServerError, err400, err401, err500, errBody, throwError)
import Servant.Server.Experimental.Auth (AuthServerData)
import System.Environment (lookupEnv)
import Text.Regex.TDFA ((=~~))
import qualified Web.JWT as JWT (JWTClaimsSet (exp, iat, iss), encodeSigned, hmacSecret, numericDate, stringOrURI)

type instance AuthServerData Protected = Bool

auth :: CarteAuth
auth =
  CarteAuth
    { lookupUser = \_ -> return True,
      authError = const err401 {errBody = "Missing header"}
    }

_healthGet :: a -> ExceptT ServerError IO NoContent
_healthGet _ = return NoContent

jwkSecret :: String
jwkSecret = "JWK_SECRET"

_lookupSecret :: ExceptT ServerError IO String
_lookupSecret = maybeToExceptT internalServerError (MaybeT $ lookupEnv jwkSecret)

mkClaims :: String -> IO JWT.JWTClaimsSet
mkClaims username = do
  currentUtcTime <- getPOSIXTime
  return
    mempty
      { JWT.iss = (JWT.stringOrURI . pack) username,
        JWT.iat = JWT.numericDate currentUtcTime,
        JWT.exp = JWT.numericDate (currentUtcTime + 3600)
      }

extendServerError :: ServerError -> LBS.ByteString -> ServerError
extendServerError serverErr msg = serverErr {errBody = errBody serverErr <> msg}

credentialsParsingError :: ServerError
credentialsParsingError = extendServerError err400 "Couldn't parse credentials from header! Please encode them by base64."

extendCredentialsParsingError :: Text -> ServerError
extendCredentialsParsingError errorMessage = extendServerError credentialsParsingError $ (encodeUtf8 . fromStrict) errorMessage

wrongUsernameOrPasswordError :: ServerError
wrongUsernameOrPasswordError = extendServerError err401 "Wrong username or password!"

internalServerError :: ServerError
internalServerError = extendServerError err500 "Internal Server Error, see server logs."

decodeCredentials :: Text -> ExceptT ServerError IO Text
decodeCredentials c = withExceptT extendCredentialsParsingError ((except . decodeBase64) c)

extractUsernameAndPassword :: Text -> (Text, Text)
extractUsernameAndPassword creds = (username, password)
  where
    [username, password] = splitOn ":" creds

getUser :: Text -> ExceptT ServerError IO User
getUser username = maybeToExceptT wrongUsernameOrPasswordError (entityVal <$> (MaybeT . runSqlite "test-data.db" $ getBy $ UniqueUsername $ unpack username))

createJwt :: User -> ExceptT ServerError IO Text
createJwt user = do
  secret <- _lookupSecret
  let key = (JWT.hmacSecret . pack) secret
  let issuer = userEmail user
  claims <- liftIO $ mkClaims issuer
  return $ JWT.encodeSigned key mempty claims

comparePasswords :: Text -> Text -> ExceptT ServerError IO ()
comparePasswords plainPassword hashedPassword = case checkPassword (mkPassword plainPassword) (PasswordHash hashedPassword) of
  PasswordCheckSuccess -> return ()
  PasswordCheckFail -> throwError wrongUsernameOrPasswordError

extractCredentials :: Text -> ExceptT ServerError IO Text
extractCredentials header = do
  _ <- header =~~ ("^Basic .*$" :: Text) :: ExceptT ServerError IO ()
  let [_, creds] = splitOn " " header
  if isBase64 creds
    then return creds
    else throwError credentialsParsingError

_getAuth :: Maybe Text -> ExceptT ServerError IO GetAuth200Response
_getAuth (Just authorizationHeader) = do
  encodedCreds <- extractCredentials authorizationHeader
  creds <- decodeCredentials encodedCreds
  let (username, password) = extractUsernameAndPassword creds
  user <- getUser username
  let hashedPassword = userPassword user
  comparePasswords password (pack hashedPassword)
  jwt <- createJwt user
  return GetAuth200Response {getAuth200ResponseJwt = Just jwt}
_getAuth Nothing = throwError $ err400 {errBody = "No Credentials found in header! Please use the `X-Authorization` field`"}

carteBackend :: CarteBackend a (ExceptT ServerError IO)
carteBackend =
  CarteBackend
    { getHealth = _healthGet,
      getAuth = _getAuth
    }
