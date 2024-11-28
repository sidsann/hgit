module CommandParser (parseInput) where

import Command (Command (..), CommandError (..))
import Data.List (find)

-- Parse the input to find the command, flags, and arguments
parseInput :: [Command] -> [String] -> Either CommandError (Command, [(String, Maybe String)], [String])
parseInput commands input = do
  (cmd, remaining) <- case parseCommand commands input of
    Just result -> Right result
    Nothing -> Left $ CommandError "Unknown Command"
  return (cmd, [], remaining)

-- Parse the command from the list of commands
parseCommand :: [Command] -> [String] -> Maybe (Command, [String])
parseCommand _ [] = Nothing
parseCommand commands (cmd : rest) =
  case find (\accCommand -> subcommand accCommand == cmd) commands of
    Just command -> Just (command, rest)
    Nothing -> Nothing
