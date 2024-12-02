{-# LANGUAGE OverloadedStrings #-}

module AddTests
  ( addTests,
  )
where

import CommandParser
    ( CommandError(CommandError),
      ParsedCommand(ParsedCommand, parsedArguments, parsedSubcommand,
                    parsedFlags) )
import Commit (buildTree)
import Control.Monad (forM_, when)
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Index (readIndexFile)
import System.Directory
  ( doesFileExist,
  )
import System.FilePath ((</>))
import Test.HUnit
  ( Test (..),
    assertBool,
    assertEqual,
    assertFailure,
  )
import Utils ( sha1Hash )
import TestUtils
    ( letCommands,
      createFiles,
      runCommand,
      runAddCommand,
      verifyIndex,
      verifyBlobExists,
      withTestRepo )

-- | Test adding a single file
testAddSingleFile :: Test
testAddSingleFile = TestCase $ withTestRepo $ \testDir -> do
  -- Create a single file
  let files = [("file1.txt", "Hello World")]
  createFiles files

  -- Run 'git add file1.txt'
  runAddCommand [] ["file1.txt"]

  -- Verify file1.txt is in index
  verifyIndex [("file1.txt", True)]

  -- Check blob file exists and matches content
  verifyBlobExists testDir "file1.txt" "Hello World"

  -- Verify SHA-1 hash
  let expectedOid = sha1Hash "Hello World"
  indexMap <- readIndexFile
  let oid = indexMap Map.! "file1.txt"
  assertEqual "OID should match SHA-1 of file content" expectedOid oid

-- | Test adding multiple files
testAddMultipleFiles :: Test
testAddMultipleFiles = TestCase $ withTestRepo $ \testDir -> do
  -- Create multiple files
  let files =
        [ ("file1.txt", "Content of file1.txt"),
          ("file2.txt", "Content of file2.txt"),
          ("dir1/file3.txt", "Content of dir1/file3.txt")
        ]
  createFiles files

  -- Run 'git add file1.txt file2.txt dir1/file3.txt'
  runAddCommand [] ["file1.txt", "file2.txt", "dir1/file3.txt"]

  -- Verify all files are in index
  verifyIndex $ map (\(f, _) -> (f, True)) files

  -- Check blob files exist and match content
  forM_ files $ uncurry (verifyBlobExists testDir)

-- | Test adding files with paths
testAddFilesWithPaths :: Test
testAddFilesWithPaths = TestCase $ withTestRepo $ \testDir -> do
  -- Create files in subdirectories
  let files =
        [ ("src/main.hs", "Code in src/main.hs"),
          ("src/utils/helpers.hs", "Code in src/utils/helpers.hs"),
          ("docs/readme.md", "Code in docs/readme.md")
        ]
  createFiles files

  -- Run 'git add src/main.hs src/utils/helpers.hs docs/readme.md'
  runAddCommand [] ["src/main.hs", "src/utils/helpers.hs", "docs/readme.md"]

  -- Verify all files are in index
  verifyIndex $ map (\(f, _) -> (f, True)) files

  -- Check blob files exist and match content
  forM_ files $ uncurry (verifyBlobExists testDir)

-- | Test adding all files with '.'
testAddAllFiles :: Test
testAddAllFiles = TestCase $ withTestRepo $ \testDir -> do
  -- Create multiple files and directories
  let files =
        [ ("file1.txt", "Content of file1.txt"),
          ("file2.txt", "Content of file2.txt"),
          ("src/main.hs", "Content of src/main.hs"),
          ("src/utils/helpers.hs", "Content of src/utils/helpers.hs"),
          ("docs/readme.md", "Content of docs/readme.md")
        ]
  createFiles files

  -- Run 'git add .'
  runAddCommand [] ["."]

  -- Verify index contains all files except .hgit
  verifyIndex $ map (\(f, _) -> (f, True)) files ++ [(".hgit", False)]

  -- Check blob files exist and match content
  forM_ files $ uncurry (verifyBlobExists testDir)

-- | Test updating tracked files with '-u'
testAddUpdateTrackedFiles :: Test
testAddUpdateTrackedFiles = TestCase $ withTestRepo $ \testDir -> do
  -- Create and add files
  let files =
        [ ("file1.txt", "Initial content of file1.txt"),
          ("file2.txt", "Initial content of file2.txt")
        ]
  createFiles files

  -- Initial add
  runAddCommand [] ["file1.txt", "file2.txt"]

  -- Modify one file
  createFiles [("file1.txt", "Updated content of file1.txt")]

  -- Read index before 'git add -u'
  indexBefore <- readIndexFile
  let initialIndexSize = Map.size indexBefore

  -- Run 'git add -u'
  runAddCommand [("update", Nothing)] []

  -- Read index after 'git add -u'
  indexAfter <- readIndexFile
  let finalIndexSize = Map.size indexAfter
  -- Ensure index size hasn't increased
  assertEqual
    "Index size should remain the same after 'git add -u'"
    initialIndexSize
    finalIndexSize

  -- Ensure only modified file has updated OID
  let oidBefore = indexBefore Map.! "file1.txt"
  let oidAfter = indexAfter Map.! "file1.txt"
  assertBool
    "OID for modified file should be updated"
    (oidBefore /= oidAfter)

  -- Ensure unmodified file's OID remains the same
  let oidFile2Before = indexBefore Map.! "file2.txt"
  let oidFile2After = indexAfter Map.! "file2.txt"
  assertEqual
    "OID for unmodified file should remain the same"
    oidFile2Before
    oidFile2After

  -- Verify SHA-1 hash of updated file
  let expectedOid = sha1Hash "Updated content of file1.txt"
  assertEqual
    "OID should match SHA-1 of updated file content"
    expectedOid
    oidAfter

-- | Test preventing addition of the .hgit directory and its contents
testAddPreventHgit :: Test
testAddPreventHgit = TestCase $ withTestRepo $ \testDir -> do
  -- Create files including within .hgit
  let files = [(".hgit/config", "Config content"), ("file1.txt", "Content of file1.txt")]
  createFiles files

  -- Run 'git add .'
  runAddCommand [] ["."]

  -- Verify .hgit/config is not in index and file1.txt is
  verifyIndex [(".hgit/config", False), ("file1.txt", True)]

  -- Check blob for file1.txt exists and matches content
  verifyBlobExists testDir "file1.txt" "Content of file1.txt"

-- | Test invalid combinations of 'git add' options
testAddInvalidCombinations :: Test
testAddInvalidCombinations = TestCase $ withTestRepo $ \testDir -> do
  -- Create a file
  createFiles [("file1.txt", "Hello World")]

  -- Attempt to combine 'git add -u' with 'git add .'
  let addCmd = letCommands !! 1 -- "add" command
  let parsedAddCmd =
        ParsedCommand
          { parsedSubcommand = addCmd,
            parsedFlags = [("update", Nothing)],
            parsedArguments = ["."]
          }
  resultAdd <- runCommand addCmd [("update", Nothing)] ["."]
  case resultAdd of
    Left (CommandError _) -> return () -- Expected to fail
    Right _ -> assertFailure "Combining 'add -u' with '.' should fail"

  -- Attempt to combine 'git add -u' with specific files
  let parsedAddCmd2 =
        ParsedCommand
          { parsedSubcommand = addCmd,
            parsedFlags = [("update", Nothing)],
            parsedArguments = ["file1.txt"]
          }
  resultAdd2 <- runCommand addCmd [("update", Nothing)] ["file1.txt"]
  case resultAdd2 of
    Left (CommandError _) -> return () -- Expected to fail
    Right _ -> assertFailure "Combining 'add -u' with specific files should fail"

-- | Test that blob files are correctly managed
testBlobFileManagement :: Test
testBlobFileManagement = TestCase $ withTestRepo $ \testDir -> do
  -- Create and add a file
  createFiles [("file1.txt", "Initial content")]
  runAddCommand [] ["file1.txt"]

  -- Read index and get initial OID
  indexMap <- readIndexFile
  let initialOid = indexMap Map.! "file1.txt"

  -- Modify the file and add again
  createFiles [("file1.txt", "Updated content")]
  runAddCommand [] ["file1.txt"]

  -- Read index and get updated OID
  indexMapAfter <- readIndexFile
  let updatedOid = indexMapAfter Map.! "file1.txt"

  -- Ensure OID has changed
  assertBool "OID should be updated after file modification" (initialOid /= updatedOid)

  -- Verify blob file for updated content exists and matches content
  verifyBlobExists testDir "file1.txt" "Updated content"

  -- Verify SHA-1 hash of updated content
  let expectedOid = sha1Hash "Updated content"
  assertEqual
    "OID should match SHA-1 of updated content"
    expectedOid
    updatedOid

  -- Re-add without modifying
  runAddCommand [] ["file1.txt"]

  -- Read index and ensure OID remains the same
  indexMapFinal <- readIndexFile
  let finalOid = indexMapFinal Map.! "file1.txt"
  assertEqual
    "OID should remain the same for unchanged file"
    updatedOid
    finalOid

-- | Collection of all add tests
testGitAdd :: Test
testGitAdd =
  TestLabel "git add tests" $
    TestList
      [ TestLabel "Add single file" testAddSingleFile,
        TestLabel "Add multiple files" testAddMultipleFiles,
        TestLabel "Add files with paths" testAddFilesWithPaths,
        TestLabel "Add all files with '.'" testAddAllFiles,
        TestLabel "Update tracked files with '-u'" testAddUpdateTrackedFiles,
        TestLabel "Prevent adding .hgit directory" testAddPreventHgit,
        TestLabel "Invalid combinations of add options" testAddInvalidCombinations,
        TestLabel "Blob file management" testBlobFileManagement
      ]

-- | Collection of all add tests to be exported
addTests :: Test
addTests = testGitAdd