{-# LANGUAGE OverloadedStrings #-}

module Branch
  ( listBranches,
    createBranch,
    deleteBranch,
  )
where

import CommandParser (CommandError (..))
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import System.Directory
  ( doesFileExist,
    listDirectory,
    removeFile, getCurrentDirectory,
  )
import System.FilePath ((</>), takeFileName)
import Utils
    ( getHgitPath,
      getHeadsPath,
      getHEADFilePath,
      readFileAsByteString,
      writeFileFromByteString,
      stringToByteString,
      getHeadCommitOid )
import qualified Data.ByteString.Char8 as BS8

-- | Lists all branches, marking the current branch with an asterisk (*)
listBranches :: IO ()
listBranches = do
  hgitPath <- getHgitPath
  headsPath <- getHeadsPath
  headFilePath <- getHEADFilePath
  currentHeadRefBS <- readFileAsByteString headFilePath
  let currentHeadRef = BS8.unpack $ BS8.strip currentHeadRefBS -- e.g., "refs/heads/main"
      currentBranch = takeFileName currentHeadRef
  branchFiles <- listDirectory headsPath
  let branches = map (\bf -> (bf, bf == currentBranch)) branchFiles
  mapM_ printBranch branches
  where
    printBranch (branch, isCurrent) =
      putStrLn $ (if isCurrent then "* " else "  ") ++ branch

-- | Creates a new branch pointing to the current commit
createBranch :: String -> IO ()
createBranch branchName = do
  repoRoot <- getCurrentDirectory 
  hgitPath <- getHgitPath
  headsPath <- getHeadsPath
  let newBranchPath = headsPath </> branchName
  branchExists <- doesFileExist newBranchPath
  when branchExists $
    throwIO $
      CommandError $
        "Branch '" ++ branchName ++ "' already exists. Please choose a different name."

  -- Get current commit OID by passing repoRoot
  currentCommitOidStr <- getHeadCommitOid repoRoot

  -- Convert String to Maybe String
  let currentCommitOid = if null currentCommitOidStr then Nothing else Just currentCommitOidStr

  case currentCommitOid of
    Nothing ->
      throwIO $
        CommandError $
          "Cannot create branch '" ++ branchName ++ "' because HEAD does not point to a valid commit."
    Just oid -> do
      -- Write the commit OID to the new branch file
      writeFileFromByteString newBranchPath $ stringToByteString oid

-- | Deletes an existing branch, ensuring it's not the current branch
deleteBranch :: String -> IO ()
deleteBranch branchName = do
  repoRoot <- getCurrentDirectory
  hgitPath <- getHgitPath
  headsPath <- getHeadsPath
  headFilePath <- getHEADFilePath
  currentHeadRefBS <- readFileAsByteString headFilePath
  let currentHeadRef = BS8.unpack $ BS8.strip currentHeadRefBS -- e.g., "refs/heads/main"
      currentBranch = takeFileName currentHeadRef

  -- Check if the branch exists
  let targetBranchPath = headsPath </> branchName
  branchExists <- doesFileExist targetBranchPath
  unless branchExists $
    throwIO $
      CommandError $
        "Branch '" ++ branchName ++ "' does not exist."

  -- Ensure the branch to delete is not the current branch
  when (branchName == currentBranch) $
    throwIO $
      CommandError $
        "Cannot delete the current branch '" ++ branchName ++ "'. Please switch to a different branch first."

  -- Delete the branch
  removeFile targetBranchPath