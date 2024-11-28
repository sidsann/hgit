module CommandParser (parseInput, Command(..), CommandError(..), ParsedCommand (..)) where

import Data.List (find, isPrefixOf)

data Command = Command 
  { subcommand :: String,
    description :: String,
    flags :: [Flag],
    args :: [String]
  } deriving (Show, Eq)

data FlagType = Optional | Required 
  deriving (Show, Eq)

data Flag = Flag 
  { longName :: String,
    shortName :: Maybe String,
    flagType :: FlagType
  } deriving (Show, Eq)

data ParsedCommand = ParsedCommand
  { parsedSubcommand :: Command
  , parsedFlags :: [(String, Maybe String)]
  , parsedArguments :: [String]
  } deriving (Show, Eq)

newtype FlagParser a = FlagParser { runFlagParser :: [(String, String)] -> Either CommandError a }

instance Functor FlagParser where
  fmap f (FlagParser p) = FlagParser $ \flags -> fmap f (p flags)

instance Applicative FlagParser where
  pure x = FlagParser $ \_ -> Right x
  (FlagParser f) <*> (FlagParser p) = FlagParser $ \flags -> do
    g <- f flags
    g <$> p flags

newtype CommandError = CommandError String

-- Parse the input to find the command, flags, and arguments
parseInput :: [Command] -> [String] -> Either CommandError ParsedCommand
parseInput commands input = do
  -- Parse the command
  (cmd, remaining) <- parseCommand commands input

  -- Parse flags and arguments
  (parsedFlags, args) <- parseFlagsAndArgs (flags cmd) remaining

  -- Return parsed result
  return $ ParsedCommand cmd parsedFlags args

-- Parse the command from the list of commands
parseCommand :: [Command] -> [String] -> Either CommandError (Command, [String])
parseCommand _ [] = Left $ CommandError "No command provided."
parseCommand commands (cmd : rest) =
  case find (\accCommand -> subcommand accCommand == cmd) commands of
    Just command -> Right (command, rest)
    Nothing -> Left $ CommandError $ "Unknown command: " ++ cmd

parseFlagsAndArgs :: [Flag] -> [String] -> Either CommandError ([(String, Maybe String)], [String])
parseFlagsAndArgs availableFlags tokens = parseTokens availableFlags tokens [] []

parseTokens :: [Flag] -> [String] -> [(String, Maybe String)] -> [String] -> Either CommandError ([(String, Maybe String)], [String])
parseTokens _ [] parsedFlags args = Right (parsedFlags, args)
parseTokens availableFlags (x:xs) parsedFlags args
  | isFlag x = handleFlag availableFlags x xs parsedFlags args
  | otherwise = parseTokens availableFlags xs parsedFlags (args ++ [x])

handleFlag :: [Flag] -> String -> [String] -> [(String, Maybe String)] -> [String] -> Either CommandError ([(String, Maybe String)], [String])
handleFlag availableFlags flagToken rest parsedFlags args = do
  flag <- matchFlag flagToken availableFlags
  case flagType flag of
    Required -> handleRequiredFlag flag rest parsedFlags args
    Optional -> handleOptionalFlag flag rest parsedFlags args

handleRequiredFlag :: Flag -> [String] -> [(String, Maybe String)] -> [String] -> Either CommandError ([(String, Maybe String)], [String])
handleRequiredFlag flag [] _ _ =
  Left $ CommandError $ "Flag " ++ longName flag ++ " requires a value, but none was provided."
handleRequiredFlag flag (value:rest) parsedFlags args =
  parseTokens [] rest ((longName flag, Just value) : parsedFlags) args

handleOptionalFlag :: Flag -> [String] -> [(String, Maybe String)] -> [String] -> Either CommandError ([(String, Maybe String)], [String])
handleOptionalFlag flag rest parsedFlags = parseTokens [] rest ((longName flag, Nothing) : parsedFlags)

isFlag :: String -> Bool
isFlag token = "--" `isPrefixOf` token || (length token > 1 && "-" `isPrefixOf` token)

matchFlag :: String -> [Flag] -> Either CommandError Flag
matchFlag token flags =
  case find (\f -> longName f == token || shortName f == Just token) flags of
    Just flag -> Right flag
    Nothing -> Left $ CommandError $ "Unknown flag: " ++ token