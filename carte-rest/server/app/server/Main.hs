import Handlers (Config (Config), auth, carteBackend, runCarteMiddlewareServer)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

requestMiddlewares :: Middleware
requestMiddlewares = logStdoutDev

main :: IO ()
main = do
  let server = carteBackend
      config = Config "http://localhost:8080/"
  runCarteMiddlewareServer config requestMiddlewares auth server
