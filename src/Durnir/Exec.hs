module Durnir.Exec (
  exec
) where

import Control.Exception (bracket)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (foldl', for_)
import Data.List (sort)
import Data.String (fromString)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.PostgreSQL.Simple (Connection, Only(..), Query, close, connectPostgreSQL, execute, execute_, query_, withTransaction)
import System.Directory (createDirectory, listDirectory)
import Durnir.Options

exec :: Options -> IO ()
exec opts = withPg (connectionString opts) $ execCmd (cmd opts)

withPg :: String -> (Connection -> IO ()) -> IO ()
withPg cstr = bracket (connectPostgreSQL $ B.pack cstr) close

execCmd :: Command -> Connection -> IO ()
execCmd command conn = do
  ensureMigrationsTable conn
  case command of
    Setup ->
      setup
    Generate name ->
      generate name
    Run ->
      run conn
    Revert ->
      revert conn
    List ->
      list conn

ensureMigrationsTable :: Connection -> IO ()
ensureMigrationsTable conn = void $ execute_ conn createMigrationsTableSql

createMigrationsTableSql :: Query
createMigrationsTableSql = "CREATE TABLE IF NOT EXISTS _durnir_migrations (name VARCHAR NOT NULL UNIQUE, run_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW());"

--
-- Setup
--

setup :: IO ()
setup = createDirectory migrationsDir

--
-- Generate
--

generate :: String -> IO ()
generate name = withValidMigrationName name generateMigrationFiles

withValidMigrationName :: String -> (String -> IO ()) -> IO ()
withValidMigrationName name f = case isValidMigrationName name of
  False ->
    putStrLn "Error: migration names must consist solely of alphanumeric characters and underscores"
  True ->
    f name

isValidMigrationName :: String -> Bool
isValidMigrationName = all (flip elem validChars)
  where
  validChars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ['_']

generateMigrationFiles :: String -> IO ()
generateMigrationFiles name = do
  prefix <- timePrefix
  let dirName = migrationsDir <> "/" <> prefix <> "_" <> name
  createDirectory dirName
  writeFile (dirName <> "/up.sql") "-- SQL"
  writeFile (dirName <> "/down.sql") "-- SQL"

timePrefix :: IO String
timePrefix = foldl' sub "" . iso8601Show <$> getCurrentTime
  where
  sub cx c = cx <> [if elem c ['-', ':', '.'] then '_' else c]

--
-- Run
--

run :: Connection -> IO ()
run conn = do
  db <- migrationsInDb conn
  disk <- migrationsOnDisk
  let pending = filter (not . flip elem db) disk
  withTransaction conn $
    runPending conn pending

runPending :: Connection -> [String] -> IO ()
runPending conn pending = case pending of
  [] ->
    putStrLn "Complete"
  name : rest -> do
    putStrLn $ "Running " <> name
    executeFile conn (migrationsDir <> "/" <> name <> "/up.sql")
    void $ execute conn "INSERT INTO _durnir_migrations (name) VALUES (?);" [name]
    runPending conn rest

--
-- Revert
--

revert :: Connection -> IO ()
revert conn = do
  db <- migrationsInDb conn
  case reverse db of
    [] ->
      putStrLn "Error: there are no migrations to revert"
    name : _ -> do
      putStrLn $ "Reverting " <> name
      withTransaction conn $ do
        executeFile conn (migrationsDir <> "/" <> name <> "/down.sql")
        void $ execute conn "DELETE FROM _durnir_migrations WHERE name = ?;" [name]

--
-- List
--

list :: Connection -> IO ()
list conn = do
  db <- migrationsInDb conn
  disk <- migrationsOnDisk
  for_ disk $ \name -> do
    let prefix = if elem name db then "+" else "-"
    putStrLn $ prefix <> " " <> name

--
-- Common
--

migrationsDir :: String
migrationsDir = "migrations"

migrationsInDb :: Connection -> IO [String]
migrationsInDb conn = map fromOnly <$> query_ conn "SELECT name from _durnir_migrations ORDER BY name;"

migrationsOnDisk :: IO [String]
migrationsOnDisk = sort <$> listDirectory migrationsDir

executeFile :: Connection -> String -> IO ()
executeFile conn fileName = do
  sql <- fromString <$> readFile fileName
  void (execute_ conn sql)
