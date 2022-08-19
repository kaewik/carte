{-# LANGUAGE OverloadedStrings #-}

module Handlers.Auth (createGetAuthHandler) where

import Carte.Types (GetAuth200Response (GetAuth200Response, getAuth200ResponseJwt))
import Control.Monad.Except (ExceptT, MonadError (throwError), withExceptT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Maybe (MaybeT, maybeToExceptT)
import qualified Data.ByteString.Lazy as LBS
import Data.Password.Bcrypt (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash), checkPassword, mkPassword)
import Data.Text (Text, pack, splitOn, stripPrefix)
import Data.Text.Encoding.Base64 (decodeBase64, isBase64)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Entities (User (userEmail, userPassword))
import Servant (ServerError, err400, err401, errBody)
import qualified Web.JWT as JWT

mkClaims :: (MonadIO m) => String -> m JWT.JWTClaimsSet
mkClaims username = do
  currentUtcTime <- liftIO getPOSIXTime
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

decodeCredentials :: (MonadIO m) => Text -> ExceptT ServerError m Text
decodeCredentials c = withExceptT extendCredentialsParsingError ((except . decodeBase64) c)

extractUsernameAndPassword :: (MonadIO m) => Text -> ExceptT ServerError m (Text, Text)
extractUsernameAndPassword creds =
  let usernameAndPassword = splitOn ":" creds
   in case usernameAndPassword of
        [] -> throwError wrongUsernameOrPasswordError
        [_] -> throwError wrongUsernameOrPasswordError
        [username, password] -> return (username, password)
        (_ : __) -> throwError wrongUsernameOrPasswordError

createJwt :: (MonadIO m) => String -> User -> ExceptT ServerError m Text
createJwt secret user = do
  let key = (JWT.hmacSecret . pack) secret
  let issuer = userEmail user
  claims <- liftIO $ mkClaims issuer
  return $ JWT.encodeSigned key mempty claims

comparePasswords :: (MonadIO m) => Text -> Text -> ExceptT ServerError m ()
comparePasswords plainPassword hashedPassword = case checkPassword (mkPassword plainPassword) (PasswordHash hashedPassword) of
  PasswordCheckSuccess -> return ()
  PasswordCheckFail -> throwError wrongUsernameOrPasswordError

extractCredentials :: (MonadIO m) => Text -> ExceptT ServerError m Text
extractCredentials header | Just creds <- stripPrefix "Basic " header = do
  if isBase64 creds
    then return creds
    else throwError credentialsParsingError
extractCredentials invalidHeader = throwError $ extendCredentialsParsingError invalidHeader

createGetAuthHandler :: MonadIO m => String -> (Text -> MaybeT m User) -> Maybe Text -> ExceptT ServerError m GetAuth200Response
createGetAuthHandler _ _ Nothing = throwError $ err400 {errBody = "No Credentials found in header! Please use the `X-Authorization` field`"}
createGetAuthHandler secret fetchUser (Just authorizationHeader) = do
  encodedCreds <- extractCredentials authorizationHeader
  creds <- decodeCredentials encodedCreds
  (username, password) <- extractUsernameAndPassword creds
  user <- maybeToExceptT wrongUsernameOrPasswordError $ fetchUser username
  let hashedPassword = userPassword user
  comparePasswords password (pack hashedPassword)
  jwt <- createJwt secret user
  return GetAuth200Response {getAuth200ResponseJwt = Just jwt}
