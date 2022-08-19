{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Handlers (ServerConfig (..), carteBackend, Config (Config), auth, runCarteMiddlewareServer) where

import Carte.API (CarteAuth (CarteAuth, authError, lookupUser), CarteBackend (CarteBackend, getAuth, getHealth), Config (Config), Protected, runCarteMiddlewareServer)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Monad.Trans (MonadIO)
import EntityHelpers.User (getByUsername)
import Handlers.Auth (createGetAuthHandler)
import Servant (NoContent (NoContent), ServerError, err401, errBody)
import Servant.Server.Experimental.Auth (AuthServerData)

newtype ServerConfig = ServerConfig {secret :: String}

type instance AuthServerData Protected = Bool

auth :: CarteAuth
auth =
  CarteAuth
    { lookupUser = \_ -> return True,
      authError = const err401 {errBody = "Missing header"}
    }

_healthGet :: (MonadIO m) => a -> ExceptT ServerError m NoContent
_healthGet _ = return NoContent

carteBackend :: (MonadIO m) => ReaderT ServerConfig m (CarteBackend a (ExceptT ServerError m))
carteBackend = do
  config <- ask
  return
    CarteBackend
      { getHealth = _healthGet,
        getAuth = createGetAuthHandler (secret config) getByUsername
      }
