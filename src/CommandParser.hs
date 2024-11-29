module CommandParser where

import Data.List (find, isPrefixOf)
import qualified Data.Set as Set
import Data.Maybe (maybeToList)
import Data.Char (isSpace)

data Command = Command
  { subcommand :: String,
    description :: String,
    flags :: [Flag],
    args :: [String]
  } deriving (Show, Eq)

data FlagType = Optional | Required
  deriving (Show, Eq, Ord)

data Flag = Flag
  { longName :: String,
    shortName :: Maybe String,
    flagType :: FlagType
  } deriving (Show, Eq, Ord)

data ParsedCommand = ParsedCommand
  { parsedSubcommand :: Command
  , parsedFlags :: [(String, Maybe String)]
  , parsedArguments :: [String]
  } deriving (Show, Eq)

newtype CommandError = CommandError String
  deriving (Show, Eq)

-- | Parses a string into tokens, respecting quoted substrings.
tokenizeInput :: String -> [String]
tokenizeInput input =
  case dropWhile isSpace input of
    "" -> []
    ('"':remaining) ->
      let (quoted, rest) = break (== '"') remaining
      in quoted : tokenizeInput (drop 1 rest)
    remaining ->
      let (token, rest) = break isSpace remaining
      in token : tokenizeInput rest

-- | Parse the input to find the command, flags, and arguments
parseInput :: [Command] -> String -> Either CommandError ParsedCommand
parseInput commands input = do
  let tokenizedInput = tokenizeInput input
  (cmd, remaining) <- parseCommand commands tokenizedInput
  (parsedFlags, args) <- parseFlagsAndArgs (flags cmd) remaining
  let requiredFlags = [flag | flag <- flags cmd, flagType flag == Required]
  let providedFlags = map fst parsedFlags
  let missingFlags = [longName flag | flag <- requiredFlags, longName flag `notElem` providedFlags]
  case missingFlags of
    [] -> return $ ParsedCommand cmd parsedFlags args
    [flag] -> Left $ CommandError $ "Flag " ++ flag ++ " requires a value, but none was provided."
    _ -> Left $ CommandError $ "Missing required flags: " ++ show missingFlags

-- | Parse the command from the list of commands
parseCommand :: [Command] -> [String] -> Either CommandError (Command, [String])
parseCommand _ [] = Left $ CommandError "No command provided."
parseCommand commands (cmd : rest) =
  case find (\accCommand -> subcommand accCommand == cmd) commands of
    Just command -> Right (command, rest)
    Nothing -> Left $ CommandError $ "Unknown command: " ++ cmd

parseFlagsAndArgs :: [Flag] -> [String] -> Either CommandError ([(String, Maybe String)], [String])
parseFlagsAndArgs availableFlags tokens = parseTokens availableFlags tokens [] [] []

parseTokens :: [Flag] -> [String] -> [(String, Maybe String)] -> [String] -> [String] -> Either CommandError ([(String, Maybe String)], [String])
parseTokens availableFlags [] parsedFlags args _ = Right (reverse parsedFlags, args)
parseTokens availableFlags (x:xs) parsedFlags args usedFlags
  | isFlag x = handleFlag availableFlags x xs parsedFlags args usedFlags
  | otherwise = parseTokens availableFlags xs parsedFlags (args ++ [x]) usedFlags

handleFlag :: [Flag] -> String -> [String] -> [(String, Maybe String)] -> [String] -> [String] -> Either CommandError ([(String, Maybe String)], [String])
handleFlag availableFlags flagToken rest parsedFlags args usedFlags = do
  flag <- matchFlag flagToken availableFlags
  let conflictingFlagNames = longName flag : maybeToList (shortName flag)
  if any (`elem` usedFlags) conflictingFlagNames
    then Left $ CommandError $ "Conflicting flags used: " ++ show (Set.fromList conflictingFlagNames)
    else case flagType flag of
      Required -> handleRequiredFlag availableFlags flag rest parsedFlags args (conflictingFlagNames ++ usedFlags)
      Optional -> handleOptionalFlag availableFlags flag rest parsedFlags args (conflictingFlagNames ++ usedFlags)

handleRequiredFlag :: [Flag] -> Flag -> [String] -> [(String, Maybe String)] -> [String] -> [String] -> Either CommandError ([(String, Maybe String)], [String])
handleRequiredFlag availableFlags flag [] _ _ _ =
  Left $ CommandError $ "Flag " ++ longName flag ++ " requires a value, but none was provided."
handleRequiredFlag availableFlags flag (value:rest) parsedFlags args usedFlags =
  parseTokens availableFlags rest ((longName flag, Just value) : parsedFlags) args usedFlags

handleOptionalFlag :: [Flag] -> Flag -> [String] -> [(String, Maybe String)] -> [String] -> [String] -> Either CommandError ([(String, Maybe String)], [String])
handleOptionalFlag availableFlags flag rest parsedFlags = parseTokens availableFlags rest ((longName flag, Nothing) : parsedFlags)

isFlag :: String -> Bool
isFlag token = "--" `isPrefixOf` token || (length token > 1 && "-" `isPrefixOf` token)

matchFlag :: String -> [Flag] -> Either CommandError Flag
matchFlag token flags =
  case filter (\f -> longName f == token || shortName f == Just token) flags of
    [] -> Left $ CommandError $ "Unknown flag: " ++ token
    [flag] -> Right flag
    _ -> Left $ CommandError $ "Ambiguous flag: " ++ token