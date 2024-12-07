{-# LANGUAGE OverloadedStrings #-}

module StatusTests
  ( statusTests,
  )
where

import Test.HUnit ( assertBool, assertFailure, Test(..) )
import TestUtils
    ( letCommands,
      withTestRepo,
      createFiles,
      runCommand,
      runAddCommand,
      runCommitCommand
    )
import CommandParser (CommandError(..), Command (subcommand))
import System.Directory (removeFile)
import System.FilePath ((</>))
import Data.List (isInfixOf)
import qualified Data.Map as Map

statusTests :: Test
statusTests = TestLabel "Status Command Tests" $ TestList
  [ TestLabel "Extended status scenario" testStatusExtendedScenario
  ]

testStatusExtendedScenario :: Test
testStatusExtendedScenario = TestCase $ withTestRepo $ \testDir -> do
  let statusCmd = head $ filter (\c -> subcommand c == "status") letCommands

  -- Step 1: Immediately after init, we should be on branch main with no changes
  result1 <- runCommand statusCmd [] []
  case result1 of
    Left (CommandError err) -> assertFailure $ "status failed unexpectedly: " ++ err
    Right output -> do
      assertBool "Should be on branch main" ("On branch main" `isInfixOf` output)
      assertBool "No changes expected initially" (not ("Changes to be committed" `isInfixOf` output))
      assertBool "No untracked files" (not ("Untracked files:" `isInfixOf` output))

  -- Step 2: Create files
  let initialFiles =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Another file"),
          ("dir/subfile1.txt", "Subdirectory file 1"),
          ("dir/subfile2.txt", "Subdirectory file 2")
        ]
  createFiles initialFiles

  -- Check status: all should be untracked now
  result2 <- runCommand statusCmd [] []
  case result2 of
    Left (CommandError err) -> assertFailure $ "status failed unexpectedly: " ++ err
    Right output -> do
      assertBool "Untracked files should appear" ("Untracked files:" `isInfixOf` output)
      mapM_ (\f -> assertBool (f ++ " should be untracked") (f `isInfixOf` output)) ["file1.txt", "file2.txt", "dir/subfile1.txt", "dir/subfile2.txt"]

  -- Step 3: Stage some files (file1.txt and all under dir/)
  runAddCommand [] ["file1.txt", "dir"]
  result3 <- runCommand statusCmd [] []
  case result3 of
    Left (CommandError err) -> assertFailure $ "status failed unexpectedly: " ++ err
    Right output -> do
      -- file1.txt and dir/subfile1.txt, dir/subfile2.txt should be under "Changes to be committed"
      assertBool "Changes to be committed" ("Changes to be committed:" `isInfixOf` output)
      mapM_ (\f -> assertBool (f ++ " should be staged") (f `isInfixOf` output)) ["file1.txt", "dir/subfile1.txt", "dir/subfile2.txt"]
      -- file2.txt should still be untracked
      assertBool "file2.txt should remain untracked" ("file2.txt" `isInfixOf` output && "Untracked files:" `isInfixOf` output)

  -- Step 4: Modify staged files without re-adding them
  createFiles [("file1.txt", "Hello World Modified")]
  createFiles [("dir/subfile1.txt", "Subdirectory file 1 modified")]

  result4 <- runCommand statusCmd [] []
  case result4 of
    Left (CommandError err) -> assertFailure $ "status failed unexpectedly: " ++ err
    Right output -> do
      -- file1.txt and dir/subfile1.txt should appear under "Changes not staged for commit"
      assertBool "Changes not staged for commit" ("Changes not staged for commit:" `isInfixOf` output)
      mapM_ (\f -> assertBool (f ++ " should be modified in WD") (f `isInfixOf` output)) ["file1.txt", "dir/subfile1.txt"]
      -- dir/subfile2.txt should remain under "Changes to be committed" since it was not modified again
      assertBool "dir/subfile2.txt should remain staged" ("Changes to be committed:" `isInfixOf` output && "dir/subfile2.txt" `isInfixOf` output)

  -- Step 5: Commit the staged changes (which includes dir/subfile2.txt but not the modified ones since we didn't re-add them)
  -- First, re-add file1.txt and dir/subfile1.txt to stage their modifications
  runAddCommand [] ["file1.txt", "dir/subfile1.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  -- After running runCommitCommand [("message", Just "Initial commit")] []
-- Check status again
-- After runCommitCommand [("message", Just "Initial commit")] []
  result5 <- runCommand statusCmd [] []
  case result5 of
    Left (CommandError err) -> assertFailure $ "status failed unexpectedly: " ++ err
    Right output -> do
        putStrLn "DEBUG: Status output after commit:"
        putStrLn output  -- Print the entire output for debugging

        -- Existing assertions
        assertBool "No changes to be committed after commit" (not ("Changes to be committed" `isInfixOf` output))
        assertBool "No changes not staged for commit after commit" (not ("Changes not staged for commit:" `isInfixOf` output))
        assertBool "file2.txt should still be untracked" ("Untracked files:" `isInfixOf` output && "file2.txt" `isInfixOf` output)

  -- Step 6: Delete a couple of tracked files and check status
  removeFile "file1.txt"
  removeFile "dir/subfile2.txt" -- remove a file in the subdir

  result6 <- runCommand statusCmd [] []
  case result6 of
    Left (CommandError err) -> assertFailure $ "status failed unexpectedly: " ++ err
    Right output -> do
      -- Deleted files should appear under "Changes not staged for commit" as deleted
      assertBool "file1.txt deleted not staged" ("deleted:   file1.txt" `isInfixOf` output)
      assertBool "dir/subfile2.txt deleted not staged" ("deleted:   dir/subfile2.txt" `isInfixOf` output)

  -- Step 7: Stage the deletions
  runAddCommand [] ["file1.txt", "dir/subfile2.txt"]

  result7 <- runCommand statusCmd [] []
  case result7 of
    Left (CommandError err) -> assertFailure $ "status failed unexpectedly: " ++ err
    Right output -> do
      -- Now the deleted files should appear under "Changes to be committed" as deleted
      assertBool "file1.txt deleted staged" ("deleted:   file1.txt" `isInfixOf` output)
      assertBool "dir/subfile2.txt deleted staged" ("deleted:   dir/subfile2.txt" `isInfixOf` output)

  -- Also check branch name again
  result8 <- runCommand statusCmd [] []
  case result8 of
    Left (CommandError err) -> assertFailure $ "status failed unexpectedly: " ++ err
    Right output -> do
      assertBool "Should still be on branch main" ("On branch main" `isInfixOf` output)