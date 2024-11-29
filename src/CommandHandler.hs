module CommandHandler (commandHandler, commands) where

import CommandParser (Command (..), CommandError (..), Flag (..), FlagType (..), ParsedCommand (..))
import Control.Exception (SomeException, try)
import Data.Text qualified (pack)
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

commands :: [Command]
commands =
  [ Command
      { subcommand = "init",
        description =
          "Creates the .hgit directory with necessary subdirectories and files, including an empty HEAD file, an empty objects directory, and refs/heads/ with an empty main file.",
        flags = [],
        args = []
      },
    Command
      { subcommand = "add",
        description =
          "Adds file(s) to the index. Supports adding individual files, updating tracked files, or adding all files in the current directory and subdirectories.",
        flags =
          [ Flag {longName = "update", shortName = Just "u", flagType = Optional}
          ],
        args = ["filename", "."]
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

commandHandler :: ParsedCommand -> IO (Either CommandError String)
commandHandler parsedCmd =
  let cmdStr = subcommand . parsedSubcommand $ parsedCmd
      flags = parsedFlags parsedCmd
      args = parsedArguments parsedCmd
   in case cmdStr of
        "init" -> handleInit
        "add" -> handleAdd flags args
        -- "commit" -> handleCommit flags args
        -- "merge" -> handleMerge flags args
        _ -> return $ Left $ CommandError $ "Unknown subcommand: " ++ cmdStr

-- | Helper function to handle exceptions and convert them to CommandError
runCommand :: IO () -> IO (Either CommandError ())
runCommand action = do
  result <- try action
  case result of
    Left (ex :: SomeException) -> return $ Left (CommandError $ show ex)
    Right _ -> return $ Right ()

-- Initialized .hgit repo, but don't have main branch pointing to any commit just yet!!
handleInit :: IO (Either CommandError String)
handleInit = do
  hgitPath <- getHgitPath
  repoExists <- doesDirectoryExist hgitPath
  if repoExists
    then return $ Right "hgit repository already exists. No action taken."
    else do
      result <- runCommand initializeRepository
      case result of
        Left err -> return $ Left err
        Right _ -> return $ Right ""
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

handleAdd :: [(String, Maybe String)] -> [String] -> IO (Either CommandError String)
handleAdd = undefined

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