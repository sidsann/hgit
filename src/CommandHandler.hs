module CommandHandler (commandHandler, commands) where

import CommandParser (Command (..), CommandError (..), Flag (..), FlagType (..), ParsedCommand (..), defaultValidate)
import Control.Exception (SomeException, try, throwIO)
import Data.Text qualified (pack)
import Index ( readIndexFile, writeIndexFile, updateIndex )
import FileIO
  ( createDirectoryIfMissing',
    createFileIfMissing,
    doesDirectoryExist,
    getHEADFilePath,
    getHeadPath,
    getHeadsPath,
    getHgitPath,
    getObjectsPath,
    writeFileFromByteString,
  )
import Hash (stringToByteString)
import Control.Monad (unless, when)

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
      }
      -- , Command
      --     { subcommand = "commit",
      --       description =
      --         "Creates a new commit from the current index with a commit message. Supports committing with a message, amending the last commit, or using the previous commit message.",
      --       flags =
      --         [ Flag { longName = "message", shortName = Just "m", flagType = Required },
      --           Flag { longName = "amend", shortName = Nothing, flagType = Optional },
      --           Flag { longName = "no-edit", shortName = Nothing, flagType = Optional }
      --         ],
      --       args = []
      --     }
      -- , Command
      --     { subcommand = "branch",
      --       description =
      --         "Lists, creates, renames, or deletes branches.",
      --       flags =
      --         [ Flag { longName = "move", shortName = Just "m", flagType = Required },
      --           Flag { longName = "delete", shortName = Just "d", flagType = Required }
      --         ],
      --       args = [ "branchname" ]
      --     }
      -- , Command
      --     { subcommand = "checkout",
      --       description =
      --         "Switches to an existing branch or creates and switches to a new branch.",
      --       flags =
      --         [ Flag { longName = "branch", shortName = Just "b", flagType = Required }
      --         ],
      --       args = [ "branchname" ]
      --     }
      -- , Command
      --     { subcommand = "switch",
      --       description =
      --         "Switches to an existing branch or creates and switches to a new branch.",
      --       flags =
      --         [ Flag { longName = "create", shortName = Just "c", flagType = Required }
      --         ],
      --       args = [ "branchname" ]
      --     }
      -- , Command
      --     { subcommand = "log",
      --       description =
      --         "Displays commit logs in reverse chronological order, showing commit hashes, branch information, authors, timestamps, and commit messages.",
      --       flags = [],
      --       args = []
      --     }
      -- , Command
      --     { subcommand = "status",
      --       description =
      --         "Shows the working tree status, including changes to be committed, changes not staged for commit, and untracked files.",
      --       flags = [],
      --       args = []
      --     }
      -- , Command
      --     { subcommand = "merge",
      --       description =
      --         "Merges another branch into the current branch.",
      --       flags = [],
      --       args = [ "branchname" ]
      --     }
      -- , Command
      --     { subcommand = "rebase",
      --       description =
      --         "Rebases the current branch onto another branch, replaying commits to create a linear history.",
      --       flags = [],
      --       args = [ "branchname" ]
      --     }
      -- , Command
      --     { subcommand = "clean",
      --       description =
      --         "Removes untracked files from the working tree.",
      --       flags = [],
      --       args = []
      --     }
      -- , Command
      --     { subcommand = "diff",
      --       description =
      --         "Shows differences between commits, commit and working tree, etc.",
      --       flags = [],
      --       args = []
      --     }
      -- , Command
      --     { subcommand = "cherry-pick",
      --       description =
      --         "Applies the changes introduced by existing commits onto the current branch.",
      --       flags = [],
      --       args = [ "commit-hash" ]
      --     }
      -- , Command
      --     { subcommand = "range-diff",
      --       description =
      --         "Compares two ranges of commits.",
      --       flags = [],
      --       args = [ "range1", "range2" ]
      --     }
      -- , Command
      --     { subcommand = "reset",
      --       description =
      --         "Resets the current HEAD to the specified state, with options to modify the index and working tree.",
      --       flags =
      --         [ Flag { longName = "hard", shortName = Just "h", flagType = Optional },
      --           Flag { longName = "soft", shortName = Just "s", flagType = Optional }
      --         ],
      --       args = [ "commit-hash" ]
      --     }
      -- , Command
      --     { subcommand = "revert",
      --       description =
      --         "Reverts some existing commits by creating new commits that undo the changes.",
      --       flags = [],
      --       args = [ "commit-hash" ]
      --     }
      -- , Command
      --     { subcommand = "restore",
      --       description =
      --         "Restores working tree files to a specified state.",
      --       flags = [],
      --       args = [ "filename" ]
      --     }
      -- , Command
      --     { subcommand = "rm",
      --       description =
      --         "Removes files from the working tree and the index.",
      --       flags =
      --         [ Flag { longName = "cached", shortName = Nothing, flagType = Optional }
      --         ],
      --       args = [ "filename" ]
      --     }
      -- , Command
      --     { subcommand = "mv",
      --       description =
      --         "Moves or renames a file, directory, or symlink.",
      --       flags = [],
      --       args = [ "source", "destination" ]
      --     }
      -- , Command
      --     { subcommand = "help",
      --       description =
      --         "Displays help information about hgit commands.",
      --       flags = [],
      --       args = []
      --     }
      -- , Command
      --     { subcommand = "config",
      --       description =
      --         "Gets and sets repository or global options like user name and email.",
      --       flags =
      --         [ Flag { longName = "user.name", shortName = Nothing, flagType = Required },
      --           Flag { longName = "user.email", shortName = Nothing, flagType = Required }
      --         ],
      --       args = [ "value" ]
      -- }
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
      throwIO $ userError ".hgit doesn't exist, call 'hgit init' first"

  -- Dispatch the command with exception handling
  result <- try $ case cmdStr of
    "init" -> handleInit
    "add" -> handleAdd flags args
    -- Add other command handlers here
    _ -> throwIO $ userError $ "Unknown subcommand: " ++ cmdStr

  -- Convert exceptions to CommandError
  case result of
    Left (ex :: SomeException) -> return $ Left (CommandError $ show ex)
    Right output -> return $ Right output

-- | Initializes the .hgit repository
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
    -- Only -u flag provided
    ([("update", Nothing)], []) -> Right ()
    -- Only '.' arg provided
    ([], ["."]) -> Right ()
    -- Only arguments provided
    ([], _:_) -> Right ()
    -- Invalid combination
    _ -> Left $ CommandError "Invalid usage of 'hgit add'. Use 'hgit add -u', 'hgit add <file>... ', or 'hgit add .'"

-- | Handles the 'add' command
handleAdd :: [(String, Maybe String)] -> [String] -> IO String
handleAdd flags args = do
  indexMap <- readIndexFile
  updatedIndexMapResult <- updateIndex indexMap args flags
  case updatedIndexMapResult of
    Left err -> throwIO $ userError $ show err
    Right updatedIndexMap -> do
      writeIndexFile updatedIndexMap
      return ""

-- if file titled index doesn't yet exist in .hgit, then create it. Once we know it exists,
-- iterate through it and load into memory the currently tracked files by OID and file path,
-- which we would initially store with absolute paths. Once loaded in, we can then add a file,
-- if the file path exists, then it is already being tracked, and if the checksum is different,
-- then create a new blob file in objects/ and assign the filepath in index file the new OID
-- this is pretty much all we need to do with add since git status can actually make sure that
-- all the files we're tracking actually still exist or not, or if they've been modified since
-- they were last added, or just untracked in general

-- handleCommit :: [(String, Maybe String)] -> [String] -> IO (Either CommandError String)
-- handleCommit flags args =
--   case lookup "--message" flags of
--     Just (Just message) -> Right $ "Committing with message: " ++ message
--     _ -> Left $ CommandError "The '--message' flag is required for 'commit'."

-- handleMerge :: [(String, Maybe String)] -> [String] -> IO (Either CommandError String)
-- handleMerge flags args =
--   case args of
--     (branch : _) -> Right $ "Merging branch: " ++ branch
--     [] -> Left $ CommandError "A branch name is required for 'merge'."