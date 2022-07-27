{-# LANGUAGE OverloadedStrings #-}

import Entities
import Database.Persist.Sqlite

main :: IO ()
main = runSqlite ":memory:" $ do runMigration migrateAll
