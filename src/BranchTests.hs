{-# LANGUAGE OverloadedStrings #-}

module BranchTests
  ( branchTests,
  )
where

import CommandHandler ()
import CommandParser
  ( Command (..),
    CommandError (..),
    ParsedCommand (..),
    defaultValidate,
  )
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Data.ByteString.Char8 qualified as BS8
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Index (readIndexFile)
import System.Directory (listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath (takeFileName, (</>))
import Test.HUnit ( assertEqual, assertFailure, Test(..) )
import TestUtils
    ( letCommands,
      createFiles,
      runCommand,
      runAddCommand,
      runCommitCommand,
      withTestRepo )
import Utils ( readFileAsByteString )

-- | Asserts that a branch command fails with a CommandError
assertBranchFailure :: [(String, Maybe String)] -> [String] -> IO ()
assertBranchFailure flags args = do
  let branchCmd = letCommands !! 3 -- "branch" command (assuming index 3)
  result <- runCommand branchCmd flags args
  case result of
    Left _ -> return () -- Expected to fail
    Right _ -> assertFailure "Expected branch command to fail, but it succeeded."

-- | Since I don't want to deal with capturing std out, this just looks at the files in heads and makes sure
-- we see what we expect
verifyBranchList :: FilePath -> String -> [String] -> IO ()
verifyBranchList testDir expectedCurrentBranch expectedBranches = do
  -- Path to refs/heads
  let headsPath = testDir </> ".hgit" </> "refs" </> "heads"

  -- List all branch files
  branchFiles <- listDirectory headsPath
  let sortedBranchFiles = sort branchFiles
      sortedExpected = sort expectedBranches
  assertEqual "Number of branches should match" sortedExpected sortedBranchFiles

  -- Read the HEAD file to determine the current branch
  let headFilePath = testDir </> ".hgit" </> "HEAD"
  headContent <- readFileAsByteString headFilePath
  let headRef = BS8.unpack $ BS8.strip headContent -- e.g., "refs/heads/main"
      currentBranch = takeFileName headRef
  assertEqual "Current branch should match" expectedCurrentBranch currentBranch

-- | Test creating a branch without any commits should fail
testBranchCreateWithoutCommit :: Test
testBranchCreateWithoutCommit = TestCase $ withTestRepo $ \_testDir -> do
  -- Attempt to create a branch
  assertBranchFailure [] ["new-branch"]

-- | Test creating a branch with an existing branch name should fail
testBranchCreateExistingBranch :: Test
testBranchCreateExistingBranch = TestCase $ withTestRepo $ \testDir -> do
  -- Create initial commit
  let files =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createFiles files
  runAddCommand [] ["file1.txt", "file2.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  -- Create a branch named "feature"
  runBranchCreateCommand "feature"

  -- Attempt to create the same branch again
  assertBranchFailure [] ["feature"]

-- | Test creating a branch with a valid name when a commit exists
testBranchCreateValid :: Test
testBranchCreateValid = TestCase $ withTestRepo $ \testDir -> do
  -- Create initial commit
  let files =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createFiles files
  runAddCommand [] ["file1.txt", "file2.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  -- Create a branch named "feature"
  runBranchCreateCommand "feature"

  -- Verify the branch was created
  let expectedBranches = ["main", "feature"]
  verifyBranchList testDir "main" expectedBranches

-- | Helper function to create a branch
runBranchCreateCommand :: String -> IO ()
runBranchCreateCommand branchName = do
  let branchCmd = letCommands !! 3 -- "branch" command
  result <- runCommand branchCmd [] [branchName]
  case result of
    Left (CommandError err) -> assertFailure $ "Branch creation failed: " ++ err
    Right _ -> return ()

-- | Test deleting a branch that is not the current branch
testBranchDeleteValid :: Test
testBranchDeleteValid = TestCase $ withTestRepo $ \testDir -> do
  -- Create initial commit
  let files =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createFiles files
  runAddCommand [] ["file1.txt", "file2.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  -- Create a branch named "feature"
  runBranchCreateCommand "feature"

  -- Delete the "feature" branch
  runBranchDeleteCommand "feature"

  -- Verify the branch was deleted
  let expectedBranches = ["main"]
  verifyBranchList testDir "main" expectedBranches

-- | Helper function to delete a branch
runBranchDeleteCommand :: String -> IO ()
runBranchDeleteCommand branchName = do
  let branchCmd = letCommands !! 3 -- "branch" command
  result <- runCommand branchCmd [("delete", Just branchName)] []
  case result of
    Left (CommandError err) -> assertFailure $ "Branch deletion failed: " ++ err
    Right _ -> return ()

-- | Test deleting the current branch should fail
testBranchDeleteCurrent :: Test
testBranchDeleteCurrent = TestCase $ withTestRepo $ \testDir -> do
  -- Create initial commit
  let files =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createFiles files
  runAddCommand [] ["file1.txt", "file2.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  -- Attempt to delete the current branch "main"
  -- Expect failure
  let branchCmd = letCommands !! 3 -- "branch" command
  result <- runCommand branchCmd [("delete", Just "main")] []
  case result of
    Left (CommandError _) -> return () -- Expected to fail
    Right _ -> assertFailure "Expected deletion of current branch to fail, but it succeeded."

-- | Test listing branches when multiple branches exist
testBranchListMultiple :: Test
testBranchListMultiple = TestCase $ withTestRepo $ \testDir -> do
  -- Create initial commit
  let files =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createFiles files
  runAddCommand [] ["file1.txt", "file2.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  -- Create multiple branches
  runBranchCreateCommand "feature"
  runBranchCreateCommand "bugfix"
  runBranchCreateCommand "release"

  -- Verify the branches
  let expectedBranches = ["main", "feature", "bugfix", "release"]
  verifyBranchList testDir "main" expectedBranches

-- | Test creating a branch after making additional commits
testBranchCreateAfterCommit :: Test
testBranchCreateAfterCommit = TestCase $ withTestRepo $ \testDir -> do
  -- Initial commit
  let files1 =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createFiles files1
  runAddCommand [] ["file1.txt", "file2.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  -- Make a second commit
  let files2 = [("file3.txt", "New file in second commit")]
  createFiles files2
  runAddCommand [] ["file3.txt"]
  runCommitCommand [("message", Just "Second commit")] []

  -- Create a branch named "feature" pointing to the second commit
  runBranchCreateCommand "feature"

  -- Verify the branches
  let expectedBranches = ["main", "feature"]
  verifyBranchList testDir "main" expectedBranches

-- | Collection of all branch tests
branchTests :: Test
branchTests =
  TestLabel "Branch Command Tests" $
    TestList
      [ TestLabel "Create Branch Without Commit" testBranchCreateWithoutCommit,
        TestLabel "Create Existing Branch" testBranchCreateExistingBranch,
        TestLabel "Create Valid Branch" testBranchCreateValid,
        TestLabel "Delete Valid Branch" testBranchDeleteValid,
        TestLabel "Delete Current Branch" testBranchDeleteCurrent,
        TestLabel "List Multiple Branches" testBranchListMultiple,
        TestLabel "Create Branch After Commit" testBranchCreateAfterCommit
      ]