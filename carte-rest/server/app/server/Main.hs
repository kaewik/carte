{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
import Carte.API
-- required dependency: wai
import Network.Wai (Middleware)
-- required dependency: wai-extra
import Network.Wai.Middleware.RequestLogger (logStdout)

import Servant (NoContent (NoContent), errBody, err401)
import           Servant.Server.Experimental.Auth   (AuthServerData)
import Control.Monad.Trans (MonadIO)

-- If you would like to not use any middlewares you could use runCarteServer instead

-- Combined middlewares
requestMiddlewares :: Middleware
requestMiddlewares = logStdout

type instance AuthServerData Protected = Bool

auth :: CarteAuth
auth =
  CarteAuth
    { lookupUser = \_ -> return True,
      authError = \request -> err401 {errBody = "Missing header"}
    }

_healthGet :: MonadIO m  => a -> m NoContent
_healthGet _ = return NoContent

carteBackend :: MonadIO m => CarteBackend a m
carteBackend = CarteBackend {getHealth = _healthGet}

-- Run a Carte server on localhost:8080
main :: IO ()
main = do
  let server = carteBackend
      config = Config "http://localhost:8080/"
  runCarteMiddlewareServer config requestMiddlewares auth server
