module CommandParser where

import Control.Exception ( Exception )
import Data.Char (isSpace)
import Data.List (find, isPrefixOf)
import Data.Maybe (maybeToList)
import Data.Set qualified as Set

data Command = Command
  { subcommand :: String,
    description :: String,
    flags :: [Flag],
    validate :: [(String, Maybe String)] -> [String] -> Either CommandError ()
  }

instance Eq Command where
  (==) :: Command -> Command -> Bool
  (Command sc1 desc1 flags1 _) == (Command sc2 desc2 flags2 _) =
    sc1 == sc2 && desc1 == desc2 && flags1 == flags2

instance Show Command where
  show (Command sc desc flags _) =
    "Command { subcommand = " ++ show sc ++ ", description = " ++ show desc ++ ", flags = " ++ show flags ++ " }"

data FlagType = NoArg | RequiresArg
  deriving (Show, Eq, Ord)

data Flag = Flag
  { longName :: String,
    shortName :: Maybe String,
    flagType :: FlagType
  }
  deriving (Show, Eq, Ord)

data ParsedCommand = ParsedCommand
  { parsedSubcommand :: Command,
    parsedFlags :: [(String, Maybe String)],
    parsedArguments :: [String]
  }
  deriving (Show, Eq)

newtype CommandError = CommandError String
  deriving (Show, Eq)

instance Exception CommandError

-- | Default validation function
defaultValidate :: [(String, Maybe String)] -> [String] -> Either CommandError ()
defaultValidate [] [] = Right ()
defaultValidate _ _ = Left $ CommandError "This command does not accept any flags or arguments."

-- | Parse the input to find the command, flags, and arguments
parseInput :: [Command] -> [String] -> Either CommandError ParsedCommand
parseInput commands input = do
  (cmd, remaining) <- parseCommand commands input
  (parsedFlags, args) <- parseFlagsAndArgs (flags cmd) remaining
  -- Call the validation function
  validate cmd parsedFlags args
  return $ ParsedCommand cmd parsedFlags args

-- | Parse the command from the list of commands
parseCommand :: [Command] -> [String] -> Either CommandError (Command, [String])
parseCommand _ [] = Left $ CommandError "No command provided."
parseCommand commands (cmd : rest) =
  case find (\accCommand -> subcommand accCommand == cmd) commands of
    Just command -> Right (command, rest)
    Nothing -> Left $ CommandError $ "Unknown command: " ++ cmd

parseFlagsAndArgs :: [Flag] -> [String] -> Either CommandError ([(String, Maybe String)], [String])
parseFlagsAndArgs availableFlags tokens = parseTokens availableFlags tokens [] [] [] False

parseTokens :: [Flag] -> [String] -> [(String, Maybe String)] -> [String] -> [String] -> Bool -> Either CommandError ([(String, Maybe String)], [String])
parseTokens _ [] parsedFlags args _ _ = Right (reverse parsedFlags, reverse args)
parseTokens availableFlags (x : xs) parsedFlags args usedFlags seenArg
  | not seenArg && isFlag x = handleFlag availableFlags x xs parsedFlags args usedFlags
  | otherwise = parseTokens availableFlags xs parsedFlags (x : args) usedFlags True

handleFlag ::
  [Flag] ->
  String ->
  [String] ->
  [(String, Maybe String)] ->
  [String] ->
  [String] ->
  Either CommandError ([(String, Maybe String)], [String])
handleFlag availableFlags flagToken rest parsedFlags args usedFlags = do
  flag <- matchFlag flagToken availableFlags
  let conflictingFlagNames = longName flag : maybeToList (shortName flag)
  if any (`elem` usedFlags) conflictingFlagNames
    then Left $ CommandError $ "Conflicting flags used: " ++ show conflictingFlagNames
    else case flagType flag of
      RequiresArg -> handleRequiredFlag availableFlags flag rest parsedFlags args (conflictingFlagNames ++ usedFlags)
      NoArg -> parseTokens availableFlags rest ((longName flag, Nothing) : parsedFlags) args (conflictingFlagNames ++ usedFlags) False

handleRequiredFlag ::
  [Flag] ->
  Flag ->
  [String] ->
  [(String, Maybe String)] ->
  [String] ->
  [String] ->
  Either CommandError ([(String, Maybe String)], [String])
handleRequiredFlag _ flag [] _ _ _ =
  Left $ CommandError $ "Flag " ++ longName flag ++ " requires a value, but none was provided."
handleRequiredFlag availableFlags flag (value : rest) parsedFlags args usedFlags =
  parseTokens availableFlags rest ((longName flag, Just value) : parsedFlags) args usedFlags False

-- Determines if a token is a flag
isFlag :: String -> Bool
isFlag ('-' : _) = True
isFlag _ = False

-- Matches a token to a Flag definition
matchFlag :: String -> [Flag] -> Either CommandError Flag
matchFlag token flags =
  let strippedToken = dropWhile (== '-') token -- Remove leading '-' or '--'
   in case filter (\f -> longName f == strippedToken || shortName f == Just strippedToken) flags of
        [] -> Left $ CommandError $ "Unknown flag: " ++ token
        [flag] -> Right flag
        _ -> Left $ CommandError $ "Ambiguous flag: " ++ token