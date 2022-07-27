{-# LANGUAGE OverloadedStrings #-}

import Carte.Entities
import Database.Persist.Sqlite

main :: IO ()
main = runSqlite ":memory:" $ do runMigration migrateAll
