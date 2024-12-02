{-# LANGUAGE OverloadedStrings #-}

module CommitTests
  ( commitTests,
  )
where

import CommandHandler
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
import TestUtils
import Utils
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
  -- Create and add files
  let files =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createFiles files

  -- Add files to index
  runAddCommand [] ["file1.txt", "file2.txt"]

  -- Commit with message
  runCommitCommand [("message", Just "Initial commit")] []

  -- Get current commit OID from HEAD
  headOid <- getHeadCommitOid testDir

  -- Verify commit
  verifyCommit testDir headOid Nothing "Initial commit"

-- | Test multiple commits and verify parent pointers
testMultipleCommits :: Test
testMultipleCommits = TestCase $ withTestRepo $ \testDir -> do
  -- Initial commit
  let files1 =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createFiles files1
  runAddCommand [] ["file1.txt", "file2.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  -- Get first commit OID
  headOid1 <- getHeadCommitOid testDir

  -- Modify a file and commit again
  let files2 = [("file1.txt", "Hello World Updated")]
  createFiles files2
  runAddCommand [] ["file1.txt"]
  runCommitCommand [("message", Just "Second commit")] []

  -- Get second commit OID
  headOid2 <- getHeadCommitOid testDir

  -- Verify second commit
  verifyCommit testDir headOid2 (Just headOid1) "Second commit"

-- | Test committing when there are no changes should create a new commit or fail
testCommitNoChanges :: Test
testCommitNoChanges = TestCase $ withTestRepo $ \testDir -> do
  -- Initial commit
  let files =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createFiles files

  -- Add files to index
  runAddCommand [] ["file1.txt", "file2.txt"]

  -- Commit with message
  runCommitCommand [("message", Just "Initial commit")] []

  -- Get first commit OID
  firstCommitOid <- getHeadCommitOid testDir

  -- Attempt to commit again without changes
  runCommitCommand [("message", Just "No changes commit")] []

  -- Get second commit OID
  secondCommitOid <- getHeadCommitOid testDir

  -- Verify second commit
  verifyCommit testDir secondCommitOid (Just firstCommitOid) "No changes commit"

-- | Test committing with additional files after initial commit
testCommitWithAdditionalFiles :: Test
testCommitWithAdditionalFiles = TestCase $ withTestRepo $ \testDir -> do
  -- Initial commit
  let files1 =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createFiles files1
  runAddCommand [] ["file1.txt", "file2.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  -- Get first commit OID
  headOid1 <- getHeadCommitOid testDir

  -- Add a new file and commit
  let files2 = [("file3.txt", "New file in second commit")]
  createFiles files2
  runAddCommand [] ["file3.txt"]
  runCommitCommand [("message", Just "Added file3.txt")] []

  -- Get second commit OID
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
  createFiles files1
  runAddCommand [] ["file1.txt", "file2.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  -- Get first commit OID
  headOid1 <- getHeadCommitOid testDir

  -- Commit 2
  let files2 = [("file1.txt", "Hello World Updated"), ("file3.txt", "New file in commit 2")]
  createFiles files2
  runAddCommand [] ["file1.txt", "file3.txt"]
  runCommitCommand [("message", Just "Second commit")] []

  -- Get second commit OID
  headOid2 <- getHeadCommitOid testDir

  -- Commit 3
  let files3 = [("file4.txt", "Another new file in commit 3")]
  createFiles files3
  runAddCommand [] ["file4.txt"]
  runCommitCommand [("message", Just "Third commit")] []

  -- Get third commit OID
  headOid3 <- getHeadCommitOid testDir

  -- Verify third commit
  verifyCommit testDir headOid3 (Just headOid2) "Third commit"

testCommitObjectStructure :: Test
testCommitObjectStructure = TestCase $ withTestRepo $ \testDir -> do
  -- Create and add files
  let files =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file"),
          ("src/main.hs", "main function"),
          ("src/utils/helpers.hs", "Helper functions")
        ]
  createFiles files

  -- Add all files
  runAddCommand [] ["file1.txt", "file2.txt", "src/main.hs", "src/utils/helpers.hs"]

  -- Commit with message
  runCommitCommand [("message", Just "Initial commit with src")] []

  -- Get current commit OID
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

testHandleDeletedFiles :: Test
testHandleDeletedFiles = TestCase $ withTestRepo $ \testDir -> do
    -- Step 1: Create initial files
    let initialFiles =
            [ ("file1.txt", "Hello World"),
              ("file2.txt", "Goodbye World"),
              ("src/main.hs", "main function"),
              ("src/utils/helpers.hs", "Helper functions")
            ]
    createFiles initialFiles
    
    -- Step 2: Add all files
    runAddCommand [] ["file1.txt", "file2.txt", "src/main.hs", "src/utils/helpers.hs"]
    
    -- Step 3: Commit initial state
    runCommitCommand [("message", Just "Initial commit with src")] []
    
    -- Step 4: Delete 'file1.txt'
    removeFile "file1.txt"
    
    -- Step 5: Add changes (which includes deletion)
    runAddCommand [] ["file1.txt"] 
    
    -- Step 6: Commit after deletion
    runCommitCommand [("message", Just "Remove file1.txt")] []
    
    -- Step 7: Get the latest commit
    headOid <- getHeadCommitOid testDir
    
    -- Step 8: Deserialize the latest commit
    commitResult <- deserializeCommit headOid
    case commitResult of
        Left err -> assertFailure $ "Failed to deserialize commit: " ++ err
        Right commit -> do
            -- Step 9: Deserialize the tree
            treeResult <- deserializeTree (treeOid commit)
            case treeResult of
                Left err -> assertFailure $ "Failed to deserialize tree: " ++ err
                Right tree -> do
                    -- Step 10: Read the updated index
                    updatedIndex <- readIndexFile
                    -- Step 11: Verify 'file1.txt' is no longer in the index and that 'file2.txt' is still there
                    assertBool "file1.txt should be removed from the index" (not $ Map.member "file1.txt" updatedIndex)
                    assertBool "file2.txt should still be in the index" (Map.member "file2.txt" updatedIndex)

testHandleDeletedDirectory :: Test
testHandleDeletedDirectory = TestCase $ withTestRepo $ \testDir -> do
    -- Step 1: Create initial files, including a directory
    let initialFiles =
            [ ("file1.txt", "Hello World"),
              ("file2.txt", "Goodbye World"),
              ("src/main.hs", "main function"),
              ("src/utils/helpers.hs", "Helper functions")
            ]
    createFiles initialFiles
    
    -- Step 2: Add all files
    runAddCommand [] ["file1.txt", "file2.txt", "src/main.hs", "src/utils/helpers.hs"]
    
    -- Step 3: Commit initial state
    runCommitCommand [("message", Just "Initial commit with src")] []
    
    -- Step 4: Delete the 'src' directory
    removeDirectoryRecursive "src"
    
    -- Step 5: Add changes (which includes directory deletion)
    runAddCommand [] ["src"] -- Attempt to add the deleted directory
    
    -- Step 6: Commit after deletion
    runCommitCommand [("message", Just "Remove src directory")] []
    
    -- Step 7: Get the latest commit
    headOid <- getHeadCommitOid testDir
    
    -- Step 8: Deserialize the latest commit
    commitResult <- deserializeCommit headOid
    case commitResult of
        Left err -> assertFailure $ "Failed to deserialize commit: " ++ err
        Right commit -> do
            -- Step 9: Deserialize the tree
            treeResult <- deserializeTree (treeOid commit)
            case treeResult of
                Left err -> assertFailure $ "Failed to deserialize tree: " ++ err
                Right tree -> do
                    -- Step 10: Read the updated index
                    updatedIndex <- readIndexFile
                    -- Step 11: Verify 'src' and its contained files are no longer in the index
                    let filesUnderSrc = ["src/main.hs", "src/utils/helpers.hs"]
                    forM_ filesUnderSrc $ \file -> do
                        assertBool (file ++ " should be removed from the index") (not $ Map.member file updatedIndex)
                    -- Verify that other files are still present
                    assertBool "file1.txt should still be in the index" (Map.member "file1.txt" updatedIndex)
                    assertBool "file2.txt should still be in the index" (Map.member "file2.txt" updatedIndex)

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