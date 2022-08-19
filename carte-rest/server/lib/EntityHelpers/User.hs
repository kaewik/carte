{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StarIsType #-}

module EntityHelpers.User (getByUsername) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Text (Text, unpack)
import Database.Persist.Sqlite (Entity (entityVal), PersistUniqueRead (getBy), runSqlite)
import Entities (Unique (UniqueUsername), User)

getByUsername :: (MonadIO m) => Text -> MaybeT m User
getByUsername username = entityVal <$> (MaybeT . liftIO $ runSqlite "test-data.db" $ getBy $ UniqueUsername $ unpack username)
