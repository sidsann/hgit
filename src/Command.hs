module Command (Command (..), CommandError (..), commandHandler) where

data Command = Command 
  { subcommand :: String,
    description :: String,
    flags :: [Flag],
    args :: [String]
  } deriving (Show, Eq)

data FlagType = Optional | Required 
  deriving (Show, Eq)

data Flag = Flag 
  { longName :: Maybe String,
    shortName :: Maybe String,
    flagType :: FlagType
  } deriving (Show, Eq)

newtype FlagParser a = FlagParser {runFlagParser :: [(String, String) -> Either CommandError a]}

newtype CommandError = CommandError String

commandHandler :: Command -> Either CommandError String
commandHandler cmd =
  -- Implement your command handling logic here
  -- Use Left (CommandError "error message") on failure
  -- Use Right outputString on success
  undefined