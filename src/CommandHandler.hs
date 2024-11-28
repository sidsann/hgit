module CommandHandler (commandHandler) where

import CommandParser (Command (..), CommandError (..), ParsedCommand (..))

commandHandler :: ParsedCommand -> Either CommandError String
commandHandler parsedCmd =
  let cmdStr = subcommand.parsedSubcommand $ parsedCmd
      flags = parsedFlags parsedCmd
      args = parsedArguments parsedCmd
  in case cmdStr of
    "commit" -> handleCommit flags args
    "merge" -> handleMerge flags args
    _ -> Left $ CommandError $ "Unknown subcommand: " ++ cmdStr

handleCommit :: [(String, Maybe String)] -> [String] -> Either CommandError String
handleCommit flags args =
  case lookup "--message" flags of
    Just (Just message) -> Right $ "Committing with message: " ++ message
    _ -> Left $ CommandError "The '--message' flag is required for 'commit'."

handleMerge :: [(String, Maybe String)] -> [String] -> Either CommandError String
handleMerge flags args =
  case args of
    (branch : _) -> Right $ "Merging branch: " ++ branch
    [] -> Left $ CommandError "A branch name is required for 'merge'."