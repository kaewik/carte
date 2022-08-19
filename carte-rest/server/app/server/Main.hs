{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Reader (runReaderT)
import Handlers (Config (Config), ServerConfig (ServerConfig), auth, carteBackend, runCarteMiddlewareServer)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment (lookupEnv)
import System.Exit (die)

requestMiddlewares :: Middleware
requestMiddlewares = logStdoutDev

jwkSecret :: String
jwkSecret = "JWK_SECRET"

lookupSecret :: IO String
lookupSecret = do
  maybeSecret <- lookupEnv jwkSecret
  case maybeSecret of
    Just secret -> return secret
    Nothing -> die "Server could not be started, expected env var JWK_SECRET."

main :: IO ()
main = do
  secret <- lookupSecret
  let serverConfig = ServerConfig secret
  let config = Config "http://localhost:8080/"
  server <- runReaderT carteBackend serverConfig
  runCarteMiddlewareServer config requestMiddlewares auth server
