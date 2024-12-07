{-# LANGUAGE OverloadedStrings #-}

module SwitchTests
  ( switchTests
  ) where

import Test.HUnit
import TestUtils
  ( withTestRepo,
    createFiles,
    runCommand,
    runAddCommand,
    runCommitCommand,
    verifyIndex,
    letCommands
  )
import System.Directory
  ( doesFileExist,
    listDirectory
  )
import System.FilePath ((</>))
import Control.Monad (when)
import Index (readIndexFile, getAllFiles)
import CommandParser (Command (..), CommandError(..))
import Data.Map.Strict qualified as Map
import Data.List (sort, isInfixOf)
import Utils (getHgitPath, stringToByteString, writeFileFromByteString, readFileAsByteString)

runSwitchCommand :: [String] -> IO (Either CommandError String)
runSwitchCommand args = do
  let switchCmd = letCommands !! 4
  runCommand switchCmd [] args


testSwitchNonExistentBranch :: Test
testSwitchNonExistentBranch = TestCase $ withTestRepo $ \testDir -> do
  let files = [("file1.txt", "Hello")]
  createFiles files
  runAddCommand [] ["file1.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  originalIndex <- readIndexFile
  originalFiles <- getAllFiles

  result <- runSwitchCommand ["no-such-branch"]
  case result of
    Left (CommandError msg) -> do
      assertBool "Error message should mention branch not existing" ("does not exist" `isInfixOf` msg)
    Right _ -> assertFailure "Expected failure when switching to non-existent branch."

  newIndex <- readIndexFile
  assertEqual "Index should not change after failed switch" originalIndex newIndex

  newFiles <- getAllFiles
  assertEqual "Working directory should not change after failed switch" (sort originalFiles) (sort newFiles)

testSwitchSameBranch :: Test
testSwitchSameBranch = TestCase $ withTestRepo $ \testDir -> do

  let files = [("file1.txt", "Hello")]
  createFiles files
  runAddCommand [] ["file1.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  originalIndex <- readIndexFile
  originalFiles <- getAllFiles

  result <- runSwitchCommand ["main"]
  case result of
    Left err -> assertFailure $ "Switching to same branch should not fail: " ++ show err
    Right _ -> return ()

  newIndex <- readIndexFile
  assertEqual "Index should not change after switching to the same branch" originalIndex newIndex

  newFiles <- getAllFiles
  assertEqual "Working directory should not change after switching to the same branch" (sort originalFiles) (sort newFiles)

testSwitchDifferentBranch :: Test
testSwitchDifferentBranch = TestCase $ withTestRepo $ \testDir -> do

  let filesMain =
        [ ("file1.txt", "Hello"),
          ("file2.txt", "World")
        ]
  createFiles filesMain
  runAddCommand [] ["file1.txt", "file2.txt"]
  runCommitCommand [("message", Just "Initial commit on main")] []

  mainIndex <- readIndexFile
  mainFiles <- getAllFiles

  let branchCmd = letCommands !! 3
  resBranch <- runCommand branchCmd [] ["feature"]
  case resBranch of
    Left (CommandError err) -> assertFailure $ "Branch creation failed: " ++ err
    Right _ -> return ()

  resSwitchFeature <- runSwitchCommand ["feature"]
  case resSwitchFeature of
    Left (CommandError err) -> assertFailure $ "Switch to feature should succeed: " ++ err
    Right _ -> return ()

  let filesFeature = [("file2.txt", "World-Updated"), ("file3.txt", "New file on feature")]
  createFiles filesFeature
  runAddCommand [] ["file2.txt", "file3.txt"]
  runCommitCommand [("message", Just "Commit on feature branch")] []

  featureIndex <- readIndexFile
  featureFiles <- getAllFiles

  assertBool "Index should differ after switching to feature and committing"
    (featureIndex /= mainIndex)
  assertBool "Files should differ after switching to feature and committing"
    (sort featureFiles /= sort mainFiles)

  resSwitchMain <- runSwitchCommand ["main"]
  case resSwitchMain of
    Left (CommandError err) -> assertFailure $ "Switching back to main should succeed: " ++ err
    Right _ -> return ()

  newIndex <- readIndexFile
  newFiles <- getAllFiles

  assertEqual "Index should match original main state after switching back" mainIndex newIndex
  assertEqual "Working directory should match original main state after switching back"
    (sort mainFiles) (sort newFiles)

switchTests :: Test
switchTests = 
  TestLabel "Switch Command Tests" $
    TestList
      [ TestLabel "Switch Non-Existent Branch" testSwitchNonExistentBranch,
        TestLabel "Switch Same Branch" testSwitchSameBranch,
        TestLabel "Switch Different Branch and Back" testSwitchDifferentBranch
      ]