module CommandHandler (commandHandler, commands) where

import CommandParser (Command (..), CommandError (..), Flag (..), FlagType (..), ParsedCommand (..), defaultValidate)
import Commit
    ( buildTree, createCommitContent, getCurrentCommitOid, updateHEAD, Commit (treeOid, timestamp, message), deserializeCommit, parentOid )
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless, when)
import Data.Map.Strict qualified as Map
import Data.Text qualified (pack)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, TimeZone)
import Index (readIndexFile, updateIndex, writeIndexFile)
import Utils
    ( doesDirectoryExist,
      createObject,
      writeFileFromByteString,
      createDirectoryIfMissing',
      createFileIfMissing,
      getHgitPath,
      getHeadsPath,
      getObjectsPath,
      getHeadPath,
      getHEADFilePath,
      stringToByteString, getHeadCommitOid )
import Branch ( listBranches, createBranch, deleteBranch )
import Data.List (intercalate)
import System.FilePath (takeDirectory)

commands :: [Command]
commands =
  [ Command
      { subcommand = "init",
        description =
          "Creates the .hgit directory with necessary subdirectories and files, including an empty HEAD file, an empty objects directory, and refs/heads/ with an empty main file.",
        flags = [],
        validate = defaultValidate
      },
    Command
      { subcommand = "add",
        description =
          "Adds file(s) to the index. Supports adding individual files, updating tracked files, or adding all files in the current directory and subdirectories.",
        flags =
          [ Flag {longName = "update", shortName = Just "u", flagType = NoArg}
          ],
        validate = validateAddCommand
      },
    Command
      { subcommand = "commit",
        description =
          "Creates a new commit from the current index with a commit message. Supports committing with a message, amending the last commit, or using the previous commit message.",
        flags =
          [Flag {longName = "message", shortName = Just "m", flagType = RequiresArg}],
        validate = validateCommitCommand
      },
    Command
      { subcommand = "branch",
        description =
          "List, create, or delete branches.\n\
          \Usage:\n\
          \  hgit branch                List all branches\n\
          \  hgit branch <branchname>   Create a new branch\n\
          \  hgit branch -d <branchname> Delete an existing branch",
        flags =
          [ Flag { longName = "delete", shortName = Just "d", flagType = RequiresArg }
          ],
        validate = validateBranchCommand
      },
      -- , Command
      --     { subcommand = "switch",
      --       description =
      --         "Switches to an existing branch or creates and switches to a new branch.",
      --       flags =
      --         [ Flag { longName = "create", shortName = Just "c", flagType = Required }
      --         ],
      --       args = [ "branchname" ]
      --     }
    Command
      { subcommand = "log",
        description =
          "Displays commit logs in reverse chronological order, showing commit hashes, timestamps, and commit messages.",
        flags = [],
        validate = defaultValidate
      }
      -- , Command
      --     { subcommand = "status",
      --       description =
      --         "Shows the working tree status, including changes to be committed, changes not staged for commit, and untracked files.",
      --       flags = [],
      --       args = []
      --     }
  ]

-- | Main command handler that dispatches commands
commandHandler :: ParsedCommand -> IO (Either CommandError String)
commandHandler parsedCmd = do
  let cmdStr = subcommand . parsedSubcommand $ parsedCmd
      flags = parsedFlags parsedCmd
      args = parsedArguments parsedCmd

  -- Check if the repository exists for all commands except 'init'
  when (cmdStr /= "init") $ do
    repoExists <- doesDirectoryExist =<< getHgitPath
    unless repoExists $
      throwIO $
        userError ".hgit doesn't exist, call 'hgit init' first"

  -- Dispatch the command with exception handling
  result <- try $ case cmdStr of
    "init" -> handleInit
    "add" -> handleAdd flags args
    "commit" -> handleCommit flags args
    "branch" -> handleBranch flags args
    "log" -> handleLog


    -- Add other command handlers here
    _ -> throwIO $ userError $ "Unknown subcommand: " ++ cmdStr

  -- Convert exceptions to CommandError
  case result of
    Left (ex :: SomeException) -> return $ Left (CommandError $ show ex)
    Right output -> return $ Right output


handleInit :: IO String
handleInit = do
  hgitPath <- getHgitPath
  repoExists <- doesDirectoryExist hgitPath
  if repoExists
    then return "hgit repository already exists. No action taken."
    else do
      initializeRepository
      return ""
  where
    initializeRepository :: IO ()
    initializeRepository = do
      createDirectoryIfMissing' =<< getObjectsPath
      createDirectoryIfMissing' =<< getHeadsPath
      headFilePath <- getHeadPath "main"
      createFileIfMissing headFilePath
      let headContent = "refs/heads/main"
      headPath <- getHEADFilePath
      writeFileFromByteString headPath $ stringToByteString headContent

validateAddCommand :: [(String, Maybe String)] -> [String] -> Either CommandError ()
validateAddCommand flags args =
  case (flags, args) of
    ([("update", Nothing)], []) -> Right ()
    ([], ["."]) -> Right ()
    ([], _ : _) -> Right ()
    _ -> Left $ CommandError "Invalid usage of 'hgit add'. Use 'hgit add -u', 'hgit add <file>... ', or 'hgit add .'"

validateCommitCommand :: [(String, Maybe String)] -> [String] -> Either CommandError ()
validateCommitCommand flags args =
  case (flags, args) of
    ([("message", Just msg)], []) | not (null msg) -> Right ()
    _ -> Left $ CommandError "Invalid usage of 'hgit commit'. Use 'hgit commit -m \"msg\"'.'"

validateBranchCommand :: [(String, Maybe String)] -> [String] -> Either CommandError ()
validateBranchCommand flags args =
  case flags of
    [("delete", Just branchName)] -> Right () -- Deleting a branch
    [] ->
      case args of
        [] -> Right () -- Listing branches
        [branchName] -> Right () -- Creating a branch
        _ -> Left $ CommandError "Invalid usage of 'hgit branch'. Use 'hgit branch', 'hgit branch <branchname>', or 'hgit branch -d <branchname>'."
    _ -> Left $ CommandError "Invalid flags for 'hgit branch'. Use '-d <branchname>' to delete a branch."

handleAdd :: [(String, Maybe String)] -> [String] -> IO String
handleAdd flags args = do
  indexMap <- readIndexFile
  updatedIndexMapResult <- updateIndex indexMap args flags
  case updatedIndexMapResult of
    Left err -> throwIO $ userError $ show err
    Right updatedIndexMap -> do
      writeIndexFile updatedIndexMap
      return ""

handleCommit :: [(String, Maybe String)] -> [String] -> IO String
handleCommit flags args = do
  let Just (Just commitMsg) = lookup "message" flags
  indexMap <- readIndexFile
  when (Map.null indexMap) $
    error "Nothing to commit. The index is empty."
  treeOid <- buildTree indexMap
  currentTime <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%s" currentTime
  parentOid <- getCurrentCommitOid
  let commitContent = createCommitContent treeOid parentOid timestamp commitMsg
  commitOid <- createObject commitContent
  updateHEAD commitOid
  return ""

handleBranch :: [(String, Maybe String)] -> [String] -> IO String
handleBranch flags args = do
  case flags of
    [("delete", Just branchName)] -> do
      deleteBranch branchName
      return $ "Deleted branch '" ++ branchName ++ "'."
    [] ->
      case args of
        [] -> do
          listBranches
          return ""
        [branchName] -> do
          createBranch branchName
          return $ "Branch '" ++ branchName ++ "' created."
        _ -> throwIO $ userError "Invalid usage of 'hgit branch'. Use 'hgit branch', 'hgit branch <branchname>', or 'hgit branch -d <branchname>'."
    _ -> throwIO $ userError "Invalid usage of 'hgit branch'. Use 'hgit branch', 'hgit branch <branchname>', or 'hgit branch -d <branchname>'."

-- | Handle the 'log' command
handleLog :: IO String
handleLog = do
  hgitPath <- getHgitPath
  let repoDir = takeDirectory hgitPath
  oidStr <- getHeadCommitOid repoDir
  let headOid = if null oidStr then Nothing else Just oidStr
  case headOid of
    Nothing -> return "No commits found."
    Just oid -> traverseCommits oid []

-- | Traverse commits starting from the given OID
traverseCommits :: String -> [Commit] -> IO String
traverseCommits oid acc = do
  commitResult <- deserializeCommit oid
  case commitResult of
    Left err -> return $ "Error reading commit " ++ oid ++ ": " ++ err
    Right commit -> do
      let newAcc = acc ++ [commit]
      case parentOid commit of
        Nothing -> formatCommits newAcc
        Just parent -> traverseCommits parent newAcc

formatCommits :: [Commit] -> IO String
formatCommits commits = do
  tz <- getCurrentTimeZone
  let reversedCommits = reverse commits
      formatted = case reversedCommits of
        [] -> []
        (latest:rest) ->
          formatCommitWithTZ tz latest True :
          map (formatCommitWithTZ tz `flip` False) rest
  return $ intercalate "\n\n" formatted

-- Change signature of formatCommitWithTZ to accept a Bool indicating whether to print HEAD
formatCommitWithTZ :: TimeZone -> Commit -> Bool -> String
formatCommitWithTZ tz commit isHead = 
  let epochStr = timestamp commit
      epochTime = read epochStr :: Int
      utcTime = posixSecondsToUTCTime (fromIntegral epochTime)
      localTime = utcToLocalTime tz utcTime
      dateString = formatTime defaultTimeLocale "%a %b %e %T %Y %z" localTime
      headIndicator = if isHead then " (HEAD -> main)" else ""
  in "commit " ++ treeOid commit ++ headIndicator ++ "\n" ++
     "Date:   " ++ dateString ++ "\n\n" ++
     "    " ++ message commit