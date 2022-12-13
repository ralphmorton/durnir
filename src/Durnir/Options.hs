module Durnir.Options (
  Options(..),
  Command(..),
  parseOptions
) where

import Options.Applicative

data Options = Options {
  connectionString :: String,
  cmd :: Command
} deriving Show

data Command
  = Setup
  | Generate String
  | Run
  | Revert
  | List
  deriving Show

parseOptions :: IO Options
parseOptions =
  execParser $
    info
      (helper <*> parser)
      (fullDesc <> progDesc "Durnir PG migrations. --help to list available commands")

parser :: Parser Options
parser = Options <$> parseConnectionString <*> parseCommand
  where
  parseConnectionString = strOption (long "conn" <> metavar "CONNECTION_STRING" <> help "PostgreSQL connection string")
  parseCommand = hsubparser (setup <> generate <> run <> revert <> list)

setup :: Mod CommandFields Command
setup = command "setup" $ info (pure Setup) (progDesc "Setup migrations")

generate :: Mod CommandFields Command
generate = command "generate" $ info generateOpts (progDesc "Generate a new migration")
  where
  generateOpts = Generate <$> strArgument (metavar "NAME" <> help "Migration name")

run :: Mod CommandFields Command
run = command "run" $ info (pure Run) (progDesc "Run pending migrations")

revert :: Mod CommandFields Command
revert = command "revert" $ info (pure Revert) (progDesc "Revert last run migration")

list :: Mod CommandFields Command
list = command "list" $ info (pure List) (progDesc "List all migrations and their status")
