{-# LANGUAGE OverloadedStrings #-}

module AuthSpec (spec) where

import Carte.Types (GetAuth200Response)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Text (Text)
import Entities (User)
import Handlers.Auth (createGetAuthHandler)
import Servant (ServerError (errBody), err400)
import Test.Hspec (Spec, describe, it, shouldReturn)

mockFetchUser :: Monad m => Maybe User -> Text -> MaybeT m User
mockFetchUser maybeUser _ = (MaybeT . return) maybeUser

fakeSecret :: String
fakeSecret = "fakeSecret"

getAuth :: (MonadIO m) => Maybe Text -> ExceptT ServerError m GetAuth200Response
getAuth = createGetAuthHandler fakeSecret (mockFetchUser Nothing)

spec :: Spec
spec =
  describe "/auth" $
    it "throws 400 if there is no X-Authorization field in the header" $ runExceptT (getAuth Nothing) `shouldReturn` Left err400 {errBody = "No Credentials found in header! Please use the `X-Authorization` field`"}
