{-# LANGUAGE OverloadedStrings #-}

import Data.Password.Bcrypt (PasswordHash (unPasswordHash), hashPassword, mkPassword)
import Data.Text (unpack)
import Database.Persist.Sqlite
import Entities (User (..), migrateAll)
import Options.Applicative (Alternative ((<|>)), execParser, flag', fullDesc, header, help, helper, info, long, progDesc, short, switch, (<**>))

type DryRun = Bool

data Mode = Populate | Migrate DryRun

main :: IO ()
main = do
  args <-
    execParser $
      info
        ( ( flag'
              Populate
              ( long "populate"
                  <> short 'p'
                  <> help "Populate the DB. Only use it once!"
              )
              <|> Migrate <$> switch (long "dry-run" <> short 'd' <> help "Prints the migration steps that would be performed.")
          )
            <**> helper
        )
        ( fullDesc
            <> progDesc "Migration tool for the test database"
            <> header "Migration Tool"
        )
  case args of
    Populate -> runSqlite "test-data.db" $ do
      passwordHash <- hashPassword $ mkPassword "foobar"
      insert_ $ User "test@test.com" $ (unpack . unPasswordHash) passwordHash
    Migrate True -> runSqlite "test-data.db" $ do
      printMigration migrateAll
    Migrate False -> runSqlite "test-data.db" $ do
      runMigration migrateAll
