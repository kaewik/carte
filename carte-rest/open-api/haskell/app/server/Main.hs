import Carte.API
-- required dependency: wai
import Network.Wai (Middleware)
-- required dependency: wai-extra
import Network.Wai.Middleware.RequestLogger (logStdout)

-- A module you wrote yourself, containing all handlers needed for the CarteBackend type.
import Carte.Handlers (carteBackend)

-- If you would like to not use any middlewares you could use runCarteServer instead

-- Combined middlewares
requestMiddlewares :: Middleware
requestMiddlewares = logStdout

-- Run a Carte server on localhost:8080
main :: IO ()
main = do
  let server = carteBackend
      config = Config "http://localhost:8080/"
  runCarteMiddlewareServer config requestMiddlewares server
