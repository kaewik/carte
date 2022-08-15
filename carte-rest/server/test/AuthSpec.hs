{-# LANGUAGE OverloadedStrings #-}

module AuthSpec (spec) where

import Carte.API (CarteBackend (getAuth))
import Control.Monad.Trans.Except (runExceptT)
import Handlers (carteBackend)
import Servant (ServerError (errBody), err400)
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = describe "/auth" $ it "throws 400 if there is no X-Authorization field in the header" $ runExceptT (getAuth carteBackend Nothing) `shouldReturn` Left err400 {errBody = "No Credentials found in header! Please use the `X-Authorization` field`"}
