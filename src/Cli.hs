module Cli (
    Options(..)
  , options
  , SubCommand(..)
  , Update(..)
)
where

import           Options.Applicative

import           Prelude
import           Utils.Options

data Options = Options FilePath SubCommand

data SubCommand
  = Add Update
  | Get Text
  | Set Update
  | Json
  deriving (Show)

type Key = Text
type Value = Text
type Inplace = Bool

data Update =
  Update Key Value Inplace
  deriving (Show)

options :: Parser Options
options =
  Options <$> fpParser <*> commandParser

fpParser :: Parser FilePath
fpParser = argStr (metavar "FILE" <> help "Input file to change in place")

commandParser :: Parser SubCommand
commandParser =
      Get  <$> subcommand "get" "Get a key" (argText (metavar "KEY"))
  <|> Add  <$> subcommand "add" "Add an item to a list" update_parser
  <|> Set  <$> subcommand "set" "Set a key" update_parser
  <|> Json <$  subcommand "json" "Output input as JSON" (pure ())
  where
    update_parser =
      Update <$> argText (metavar "KEY")
             <*> argText (metavar "VALUE")
             <*> switch (long "inplace" <> help "Change file inplace")
