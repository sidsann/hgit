module CommandHandler (commandHandler, commands) where

import CommandParser (Command (..), CommandError (..), Flag (..), FlagType (..), ParsedCommand (..), defaultValidate)
import Commit (buildTree, createCommitContent, getCurrentCommitOid, updateHEAD, Commit, parentOid, traverseCommits, checkoutCommit, anyModifiedFile, uncommittedChangesExist)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless, when)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Index (readIndexFile, updateIndex, writeIndexFile, getAllFiles)
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
      stringToByteString,
      getHeadCommitOid, readFileAsByteString
    )
import Branch ( listBranches, createBranch, deleteBranch )
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesFileExist, listDirectory)
import qualified Data.ByteString.Char8 as BS8
import Status
    ( getCurrentBranchName,
      getHEADTreeMap,
      buildWorkingDirectoryMap,
      computeChangesToBeCommitted,
      computeChangesNotStaged,
      computeUntrackedFiles,
      formatStatusOutput )

commands :: [Command]
commands =
  [ Command
      { subcommand = "init",
        description =
          "Creates the .hgit directory...",
        flags = [],
        validate = defaultValidate
      },
    Command
      { subcommand = "add",
        description = "Adds file(s) to the index.",
        flags =
          [ Flag {longName = "update", shortName = Just "u", flagType = NoArg}
          ],
        validate = validateAddCommand
      },
    Command
      { subcommand = "commit",
        description = "Creates a new commit from the current index...",
        flags =
          [Flag {longName = "message", shortName = Just "m", flagType = RequiresArg}],
        validate = validateCommitCommand
      },
    Command
      { subcommand = "branch",
        description =
          "List, create, or delete branches...",
        flags =
          [ Flag { longName = "delete", shortName = Just "d", flagType = RequiresArg }
          ],
        validate = validateBranchCommand
      },
    Command
      { subcommand = "switch",
        description =
          "Switches to an existing branch. Usage: 'hgit switch <branchname>'",
        flags = [],
        validate = validateSwitchCommand
      },
    Command
      { subcommand = "log",
        description = "Displays commit logs...",
        flags = [],
        validate = defaultValidate
      },
          Command
      { subcommand = "status",
        description = "Show the working tree status.",
        flags = [],
        validate = defaultValidate
      }
  ]

commandHandler :: ParsedCommand -> IO (Either CommandError String)
commandHandler parsedCmd = do
  let cmdStr = subcommand . parsedSubcommand $ parsedCmd
      flags = parsedFlags parsedCmd
      args = parsedArguments parsedCmd

  when (cmdStr /= "init") $ do
    repoExists <- doesDirectoryExist =<< getHgitPath
    unless repoExists $
      throwIO $
        userError ".hgit doesn't exist, call 'hgit init' first"

  result <- try $ case cmdStr of
    "init" -> handleInit
    "add" -> handleAdd flags args
    "commit" -> handleCommit flags args
    "branch" -> handleBranch flags args
    "log" -> handleLogCommand
    "switch" -> handleSwitchCommand args
    "status" -> handleStatus

    _ -> throwIO $ userError $ "Unknown subcommand: " ++ cmdStr

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
    [("delete", Just _)] -> Right () -- Deleting a branch
    [] ->
      case args of
        [] -> Right () -- Listing branches
        [_] -> Right () -- Creating a branch
        _ -> Left $ CommandError "Invalid usage of 'hgit branch'."
    _ -> Left $ CommandError "Invalid flags for 'hgit branch'."

validateSwitchCommand :: [(String, Maybe String)] -> [String] -> Either CommandError ()
validateSwitchCommand _ args =
  case args of
    [_] -> Right ()
    _ -> Left $ CommandError "Invalid usage of 'hgit switch'."

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
handleCommit flags _args = do
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
        _ -> throwIO $ userError "Invalid usage of 'hgit branch'."
    _ -> throwIO $ userError "Invalid usage of 'hgit branch'."

handleLogCommand :: IO String
handleLogCommand = do
  hgitPath <- getHgitPath
  let repoDir = takeDirectory hgitPath
  oidStr <- getHeadCommitOid repoDir
  let headOid = if null oidStr then Nothing else Just oidStr
  case headOid of
    Nothing -> return "No commits found."
    Just oid -> traverseCommits oid []

handleSwitchCommand :: [String] -> IO String
handleSwitchCommand [branchName] = do
  headsPath <- getHeadsPath
  branchFiles <- listDirectory headsPath

  unless (branchName `elem` branchFiles) $ 
    throwIO $ CommandError $ "Branch '" ++ branchName ++ "' does not exist"

  changesExist <- uncommittedChangesExist
  when changesExist $
    throwIO $ CommandError "You have uncommitted changes. Please commit or discard them before switching branches."

  headPath <- getHEADFilePath
  writeFileFromByteString headPath (stringToByteString $ "refs/heads/" ++ branchName)

  let branchRefPath = headsPath </> branchName
  commitOidBS <- readFileAsByteString branchRefPath
  let commitOid = BS8.unpack $ BS8.strip commitOidBS

  checkoutCommit commitOid
  return $ "Switched to branch '" ++ branchName ++ "'"
handleSwitchCommand _ = error "Invalid usage. This should never happen due to validateSwitchCommand."

handleStatus :: IO String
handleStatus = do
  branchName <- getCurrentBranchName

  headTreeMap <- getHEADTreeMap

  indexMap <- readIndexFile

  workingFiles <- getAllFiles
  workingMap <- buildWorkingDirectoryMap workingFiles

  let changesToCommit = computeChangesToBeCommitted headTreeMap indexMap
  let changesNotStaged = computeChangesNotStaged indexMap workingMap
  let untrackedFiles = computeUntrackedFiles headTreeMap indexMap workingMap

  return $ formatStatusOutput branchName changesToCommit changesNotStaged untrackedFiles