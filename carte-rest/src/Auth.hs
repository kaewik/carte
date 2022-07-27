module Auth where

import Data.UUID.V4 as UUID
import Data.Aeson as DA
import Data.Aeson.Key as DAK

import Foundation
import Yesod.Core

getAuthR :: Handler Value
getAuthR = do
    token <- liftIO $ UUID.nextRandom
    let token_key = DAK.fromString "token"
    return $ DA.object [token_key .= show token]
