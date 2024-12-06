{-# LANGUAGE OverloadedStrings #-}

module CommitTests
  ( commitTests,
  )
where

import CommandHandler ()
import CommandParser
  ( Command (..),
    CommandError (..),
    ParsedCommand (..),
    defaultValidate,
  )
import Commit
  ( Commit (..),
    Tree (..),
    buildTree,
    deserializeCommit,
    deserializeTree,
  )
import Control.Monad (forM_, when)
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Index (readIndexFile)
import System.FilePath (splitDirectories, takeDirectory, takeFileName, (</>))
import Test.HUnit
    ( assertBool, assertEqual, assertFailure, Test(..) )
import TestUtils
    ( letCommands,
      createFiles,
      runCommand,
      runAddCommand,
      runCommitCommand,
      withTestRepo )
import Utils ( getHeadCommitOid )
import System.Directory (removeFile, removeDirectoryRecursive)

-- | Asserts that a commit command fails with a CommandError
assertCommitFailure :: [(String, Maybe String)] -> [String] -> IO ()
assertCommitFailure flags args = do
  let commitCmd = letCommands !! 2 -- "commit" command
  result <- runCommand commitCmd flags args
  case result of
    Left _ -> return () -- Expected to fail
    Right _ -> assertFailure "Expected commit to fail, but it succeeded."

-- | Verifies that a commit contains the expected properties
verifyCommit :: FilePath -> String -> Maybe String -> String -> IO ()
verifyCommit testDir commitOid expectedParentOid expectedMessage = do
  -- Deserialize commit
  commitResult <- deserializeCommit commitOid
  case commitResult of
    Left err -> assertFailure $ "Failed to deserialize commit: " ++ err
    Right commit -> do
      -- Verify parent pointer
      assertEqual "Commit parent OID mismatch" expectedParentOid (parentOid commit)
      -- Verify commit message
      assertEqual "Commit message mismatch" expectedMessage (message commit)
      -- Verify commit references correct tree
      indexMap <- readIndexFile
      expectedTreeOid <- buildTree indexMap
      assertEqual "Commit tree OID mismatch" expectedTreeOid (treeOid commit)

-- | Helper function to create and add files
createAndAddFiles :: [(FilePath, String)] -> IO ()
createAndAddFiles files = do
  createFiles files
  let filePaths = map fst files
  runAddCommand [] filePaths

-- | Helper function to perform commit with a message
commitWithMessage :: String -> IO ()
commitWithMessage msg = do
  runCommitCommand [("message", Just msg)] []

-- | Helper function to perform commit with flags and arguments
commitWithFlags :: [(String, Maybe String)] -> [String] -> IO ()
commitWithFlags = runCommitCommand

-- | Collection of all commit tests
commitTests :: Test
commitTests =
  TestLabel "Commit Command Tests" $
    TestList
      [ TestLabel "Commit Without Message" testCommitWithoutMessage,
        TestLabel "Commit With Empty Message" testCommitWithEmptyMessage,
        TestLabel "Initial Commit" testInitialCommit,
        TestLabel "Multiple Commits" testMultipleCommits,
        TestLabel "Commit Without Changes" testCommitNoChanges,
        TestLabel "Commit With Additional Files" testCommitWithAdditionalFiles,
        TestLabel "Commit Sequence" testCommitSequence,
        TestLabel "Commit Object Structure with Subtrees" testCommitObjectStructure,
        TestLabel "Tracked File Deletion Between Commits" testHandleDeletedFiles,
        TestLabel "Tracked Directory Deletion Between Commits" testHandleDeletedDirectory
      ]

-- | Test committing without -m flag should fail
testCommitWithoutMessage :: Test
testCommitWithoutMessage = TestCase $ withTestRepo $ \_testDir -> do
  assertCommitFailure [] []

-- | Test committing with empty message should fail
testCommitWithEmptyMessage :: Test
testCommitWithEmptyMessage = TestCase $ withTestRepo $ \_testDir -> do
  assertCommitFailure [("message", Just "")] []

-- | Test initial commit without previous commits
testInitialCommit :: Test
testInitialCommit = TestCase $ withTestRepo $ \testDir -> do
  let initialFiles =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createAndAddFiles initialFiles
  commitWithMessage "Initial commit"
  headOid <- getHeadCommitOid testDir
  verifyCommit testDir headOid Nothing "Initial commit"

-- | Test multiple commits and verify parent pointers
testMultipleCommits :: Test
testMultipleCommits = TestCase $ withTestRepo $ \testDir -> do
  -- Initial commit
  let initialFiles =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createAndAddFiles initialFiles
  commitWithMessage "Initial commit"
  headOid1 <- getHeadCommitOid testDir

  -- Second commit
  let modifiedFiles = [("file1.txt", "Hello World Updated")]
  createAndAddFiles modifiedFiles
  commitWithMessage "Second commit"
  headOid2 <- getHeadCommitOid testDir

  -- Verify second commit
  verifyCommit testDir headOid2 (Just headOid1) "Second commit"

-- | Test committing when there are no changes should create a new commit or fail
testCommitNoChanges :: Test
testCommitNoChanges = TestCase $ withTestRepo $ \testDir -> do
  let initialFiles =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createAndAddFiles initialFiles
  commitWithMessage "Initial commit"
  firstCommitOid <- getHeadCommitOid testDir

  -- Attempt to commit again without changes
  commitWithMessage "No changes commit"
  secondCommitOid <- getHeadCommitOid testDir

  -- Verify second commit
  verifyCommit testDir secondCommitOid (Just firstCommitOid) "No changes commit"

-- | Test committing with additional files after initial commit
testCommitWithAdditionalFiles :: Test
testCommitWithAdditionalFiles = TestCase $ withTestRepo $ \testDir -> do
  -- Initial commit
  let initialFiles =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createAndAddFiles initialFiles
  commitWithMessage "Initial commit"
  headOid1 <- getHeadCommitOid testDir

  -- Add a new file and commit
  let newFiles = [("file3.txt", "New file in second commit")]
  createAndAddFiles newFiles
  commitWithMessage "Added file3.txt"
  headOid2 <- getHeadCommitOid testDir

  -- Verify second commit
  verifyCommit testDir headOid2 (Just headOid1) "Added file3.txt"

-- | Test committing multiple times and verifying parent pointers and tree structure
testCommitSequence :: Test
testCommitSequence = TestCase $ withTestRepo $ \testDir -> do
  -- Commit 1
  let files1 =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createAndAddFiles files1
  commitWithMessage "Initial commit"
  headOid1 <- getHeadCommitOid testDir

  -- Commit 2
  let files2 =
        [ ("file1.txt", "Hello World Updated"),
          ("file3.txt", "New file in commit 2")
        ]
  createAndAddFiles files2
  commitWithMessage "Second commit"
  headOid2 <- getHeadCommitOid testDir

  -- Commit 3
  let files3 = [("file4.txt", "Another new file in commit 3")]
  createAndAddFiles files3
  commitWithMessage "Third commit"
  headOid3 <- getHeadCommitOid testDir

  -- Verify third commit
  verifyCommit testDir headOid3 (Just headOid2) "Third commit"

-- | Test commit object structure with subtrees
testCommitObjectStructure :: Test
testCommitObjectStructure = TestCase $ withTestRepo $ \testDir -> do
  let files =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file"),
          ("src/main.hs", "main function"),
          ("src/utils/helpers.hs", "Helper functions")
        ]
  createAndAddFiles files
  commitWithMessage "Initial commit with src"
  headOid <- getHeadCommitOid testDir

  -- Deserialize commit
  commitResult <- deserializeCommit headOid
  case commitResult of
    Left err -> assertFailure $ "Failed to deserialize commit: " ++ err
    Right commit -> do
      -- Verify commit
      assertEqual "Commit should have no parent" Nothing (parentOid commit)
      assertEqual "Commit message should match" "Initial commit with src" (message commit)

      -- Deserialize tree
      treeResult <- deserializeTree (treeOid commit)
      case treeResult of
        Left err -> assertFailure $ "Failed to deserialize tree: " ++ err
        Right tree -> do
          -- Verify tree structure
          let treeEntriesMap = Map.fromList [(name, oid) | (_, oid, name) <- treeEntries tree]
          forM_ files $ \(file, _) -> do
            let fileName = takeFileName file
            if takeDirectory file == "."
              then assertBool ("Tree should contain " ++ fileName) (fileName `Map.member` treeEntriesMap)
              else do
                let dirName = head $ splitDirectories (takeDirectory file)
                assertBool ("Tree should contain subtree " ++ dirName) (dirName `Map.member` treeEntriesMap)

-- | Test handling deletion of tracked files between commits
testHandleDeletedFiles :: Test
testHandleDeletedFiles = TestCase $ withTestRepo $ \testDir -> do
    -- Initial setup
    let initialFiles =
            [ ("file1.txt", "Hello World"),
              ("file2.txt", "Goodbye World"),
              ("src/main.hs", "main function"),
              ("src/utils/helpers.hs", "Helper functions")
            ]
    createAndAddFiles initialFiles
    commitWithMessage "Initial commit with src"

    -- Delete 'file1.txt'
    removeFile "file1.txt"

    -- Add changes (which includes deletion)
    runAddCommand [] ["file1.txt"]

    -- Commit after deletion
    commitWithMessage "Remove file1.txt"
    headOid <- getHeadCommitOid testDir

    -- Deserialize the latest commit
    commitResult <- deserializeCommit headOid
    case commitResult of
        Left err -> assertFailure $ "Failed to deserialize commit: " ++ err
        Right commit -> do
            -- Deserialize the tree
            treeResult <- deserializeTree (treeOid commit)
            case treeResult of
                Left err -> assertFailure $ "Failed to deserialize tree: " ++ err
                Right tree -> do
                    -- Read the updated index
                    updatedIndex <- readIndexFile
                    -- Verify 'file1.txt' is no longer in the index and that 'file2.txt' is still there
                    assertBool "file1.txt should be removed from the index" (not $ Map.member "file1.txt" updatedIndex)
                    assertBool "file2.txt should still be in the index" (Map.member "file2.txt" updatedIndex)

-- | Test handling deletion of tracked directories between commits
testHandleDeletedDirectory :: Test
testHandleDeletedDirectory = TestCase $ withTestRepo $ \testDir -> do
    -- Initial setup
    let initialFiles =
            [ ("file1.txt", "Hello World"),
              ("file2.txt", "Goodbye World"),
              ("src/main.hs", "main function"),
              ("src/utils/helpers.hs", "Helper functions")
            ]
    createAndAddFiles initialFiles
    commitWithMessage "Initial commit with src"

    -- Delete the 'src' directory
    removeDirectoryRecursive "src"

    -- Add changes (which includes directory deletion)
    runAddCommand [] ["src"] -- Attempt to add the deleted directory

    -- Commit after deletion
    commitWithMessage "Remove src directory"
    headOid <- getHeadCommitOid testDir

    -- Deserialize the latest commit
    commitResult <- deserializeCommit headOid
    case commitResult of
        Left err -> assertFailure $ "Failed to deserialize commit: " ++ err
        Right commit -> do
            -- Deserialize the tree
            treeResult <- deserializeTree (treeOid commit)
            case treeResult of
                Left err -> assertFailure $ "Failed to deserialize tree: " ++ err
                Right tree -> do
                    -- Read the updated index
                    updatedIndex <- readIndexFile
                    -- Verify 'src' and its contained files are no longer in the index
                    let filesUnderSrc = ["src/main.hs", "src/utils/helpers.hs"]
                    forM_ filesUnderSrc $ \file -> do
                        assertBool (file ++ " should be removed from the index") (not $ Map.member file updatedIndex)
                    -- Verify that other files are still present
                    assertBool "file1.txt should still be in the index" (Map.member "file1.txt" updatedIndex)
                    assertBool "file2.txt should still be in the index" (Map.member "file2.txt" updatedIndex)