{-# LANGUAGE OverloadedStrings #-}

import CommandHandler
import CommandParser
  ( Command (Command, flags, subcommand),
    CommandError (..),
    Flag (..),
    FlagType (..),
    ParsedCommand
      ( ParsedCommand,
        parsedArguments,
        parsedFlags,
        parsedSubcommand
      ),
    defaultValidate,
    parseCommand,
    parseFlagsAndArgs,
    parseInput,
  )
import Commit
  ( Commit (..),
    Tree (..),
    buildTree,
    createCommitContent,
    deserializeCommit,
    deserializeTree,
    getCurrentCommitOid,
    updateHEAD,
  )
import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Bifunctor qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding.Error (UnicodeException)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import FileIO
  ( createDirectoryIfMissing',
    createFileIfMissing,
    doesDirectoryExist,
    getHEADFilePath,
    getHeadPath,
    getHeadsPath,
    getHgitPath,
    getIndexFilePath,
    getObjectsPath,
    getRefsPath,
    readFileAsByteString,
    writeFileFromByteString,
  )
import Hash (byteStringToText, compress, decompress, sha1Hash, stringToByteString, textToByteString)
import Index (readIndexFile, writeIndexFile)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    listDirectory,
    removeDirectoryRecursive,
    setCurrentDirectory,
  )
import System.FilePath (makeRelative, normalise, splitDirectories, takeDirectory, takeFileName, (</>))
import Test.HUnit
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Args (maxSuccess),
    Gen,
    Property,
    Testable,
    chooseInt,
    elements,
    forAll,
    frequency,
    listOf,
    listOf1,
    quickCheck,
    quickCheckWith,
    stdArgs,
    (==>),
  )

-- | Validation function that allows any flags and arguments
testValidate :: [(String, Maybe String)] -> [String] -> Either CommandError ()
testValidate _ _ = Right ()

letCommands :: [Command]
letCommands = commands

-- | Helper function to create multiple files with specified content
createFiles :: [(FilePath, String)] -> IO ()
createFiles = mapM_ (\(f, content) -> createDirectoryIfMissing True (takeDirectory f) >> writeFile f content)

-- | Runs a specified command with given flags and arguments
runCommand :: Command -> [(String, Maybe String)] -> [String] -> IO (Either CommandError String)
runCommand cmd flags args = do
  let parsedCmd =
        ParsedCommand
          { parsedSubcommand = cmd,
            parsedFlags = flags,
            parsedArguments = args
          }
  commandHandler parsedCmd

-- | Runs the 'git add' command with given flags and arguments
runAddCommand :: [(String, Maybe String)] -> [String] -> IO ()
runAddCommand flags args = do
  let addCmd = letCommands !! 1 -- "add" command
  resultAdd <- runCommand addCmd flags args
  case resultAdd of
    Left (CommandError err) -> assertFailure $ "Add command failed: " ++ err
    Right _ -> return ()

-- | Runs the 'git commit' command with given flags and arguments
runCommitCommand :: [(String, Maybe String)] -> [String] -> IO ()
runCommitCommand flags args = do
  let commitCmd = letCommands !! 2 -- "commit" command
  resultCommit <- runCommand commitCmd flags args
  case resultCommit of
    Left (CommandError err) -> assertFailure $ "Commit command failed: " ++ err
    Right _ -> return ()

-- | Verifies that specified files are present or absent in the index
verifyIndex :: [(FilePath, Bool)] -> IO ()
verifyIndex checks = do
  indexMap <- readIndexFile
  forM_ checks $ \(file, shouldBePresent) -> do
    let normalizedFile = normalise file
    if shouldBePresent
      then assertBool (file ++ " should be in index") (Map.member normalizedFile indexMap)
      else assertBool (file ++ " should not be in index") (not $ Map.member normalizedFile indexMap)

-- | Verifies that blob files exist for specified files and match expected content
verifyBlobExists :: FilePath -> FilePath -> String -> IO ()
verifyBlobExists testDir file content = do
  indexMap <- readIndexFile
  let oid = indexMap Map.! file
  let (dirName, fileName) = splitAt 2 oid
  let blobPath = testDir </> ".hgit" </> "objects" </> dirName </> fileName
  blobExists <- doesFileExist blobPath
  assertBool ("Blob file should exist for " ++ file) blobExists
  let expectedOid = sha1Hash $ BS8.pack content
  assertEqual
    ("OID should match SHA-1 of " ++ file)
    expectedOid
    oid

-- | Helper function to initialize a test repository
withTestRepo :: (FilePath -> IO ()) -> IO ()
withTestRepo action = do
  originalDir <- getCurrentDirectory
  let testDir = originalDir </> "test-repo"

  -- Remove testDir if it exists
  testDirExists <- doesDirectoryExist testDir
  when testDirExists $ removeDirectoryRecursive testDir

  -- Create and navigate to testDir
  createDirectoryIfMissing True testDir
  setCurrentDirectory testDir

  -- Initialize repository
  let initCmd = head letCommands -- "init" command
  resultInit <- runCommand initCmd [] []
  case resultInit of
    Left (CommandError err) -> do
      setCurrentDirectory originalDir
      removeDirectoryRecursive testDir
      assertFailure $ "Initialization failed: " ++ err
    Right _ -> do
      action testDir
      setCurrentDirectory originalDir
      removeDirectoryRecursive testDir

-- Parsing-related Tests
testParseCommand :: Test
testParseCommand =
  TestList
    [ "Valid Command 'add'"
        ~: parseCommand letCommands ["add"]
        ~?= Right (letCommands !! 1, []),
      "Valid Command 'init'"
        ~: parseCommand letCommands ["init"]
        ~?= Right (head letCommands, []),
      "Invalid Command"
        ~: parseCommand letCommands ["status"]
        ~?= Left (CommandError "Unknown command: status")
    ]

testParseFlagsAndArgs :: Test
testParseFlagsAndArgs =
  TestList
    [ "Parse no-arg flag (--update)"
        ~: parseFlagsAndArgs (flags $ letCommands !! 1) ["--update"]
        ~?= Right ([("update", Nothing)], []),
      "Parse short flag (-u)"
        ~: parseFlagsAndArgs (flags $ letCommands !! 1) ["-u"]
        ~?= Right ([("update", Nothing)], []),
      "Parse arguments"
        ~: parseFlagsAndArgs (flags $ letCommands !! 1) ["file1.txt", "file2.txt"]
        ~?= Right ([], ["file1.txt", "file2.txt"]),
      "Parse flags and arguments"
        ~: parseFlagsAndArgs (flags $ letCommands !! 1) ["--update", "file1.txt"]
        ~?= Right ([("update", Nothing)], ["file1.txt"])
    ]

testParseInput :: Test
testParseInput =
  TestList
    [ "Parse 'add' with no flags or args"
        ~: parseInput letCommands "add"
        ~?= Left (CommandError "Invalid usage of 'hgit add'. Use 'hgit add -u', 'hgit add <file>... ', or 'hgit add .'"),
      "Parse 'add' with --update flag"
        ~: parseInput letCommands "add --update"
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = letCommands !! 1,
                parsedFlags = [("update", Nothing)],
                parsedArguments = []
              }
          ),
      "Parse 'add' with files"
        ~: parseInput letCommands "add file1.txt file2.txt"
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = letCommands !! 1,
                parsedFlags = [],
                parsedArguments = ["file1.txt", "file2.txt"]
              }
          ),
      "Parse 'add' with flags and files"
        ~: parseInput letCommands "add -u file1.txt"
        ~?= Left (CommandError "Invalid usage of 'hgit add'. Use 'hgit add -u', 'hgit add <file>... ', or 'hgit add .'"),
      "Parse 'init' with no flags or args"
        ~: parseInput letCommands "init"
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = head letCommands,
                parsedFlags = [],
                parsedArguments = []
              }
          ),
      "Parse 'init' with unexpected args"
        ~: parseInput letCommands "init extra"
        ~?= Left (CommandError "This command does not accept any flags or arguments.")
    ]

-- | Validation function to ensure no flag conflicts
validateCommand :: Command -> Bool
validateCommand cmd =
  let flagNames = map longName (flags cmd)
      shortNames = mapMaybe shortName (flags cmd)
      longNamesSet = Set.fromList flagNames
      shortNamesSet = Set.fromList shortNames
      uniqueLongNames = length flagNames == Set.size longNamesSet
      uniqueShortNames = length shortNames == Set.size shortNamesSet
      noOverlap = Set.null $ Set.intersection longNamesSet shortNamesSet
   in uniqueLongNames && uniqueShortNames && noOverlap

-- | Property to test that parseInput correctly parses commands
prop_parseInput_correct :: Property
prop_parseInput_correct = forAll arbitrary $ \cmd ->
  validateCommand cmd ==>
    let flagsWithValues = map assignFlagValue (flags cmd)
        inputString = constructInputString cmd flagsWithValues []
        expectedParsedFlags = map (Data.Bifunctor.first longName) flagsWithValues
        expectedParsedCommand =
          ParsedCommand
            { parsedSubcommand = cmd,
              parsedFlags = expectedParsedFlags,
              parsedArguments = []
            }
     in case parseInput [cmd] inputString of
          Right parsedCmd -> parsedCmd == expectedParsedCommand
          Left err -> error $ "Parser failed with error: " ++ show err
  where
    assignFlagValue :: Flag -> (Flag, Maybe String)
    assignFlagValue flag = case flagType flag of
      RequiresArg -> (flag, Just "value")
      NoArg -> (flag, Nothing)

constructInputString :: Command -> [(Flag, Maybe String)] -> [String] -> String
constructInputString cmd flagValues argsList =
  unwords $ [subcommand cmd] ++ flagStrings ++ argsList
  where
    flagStrings = concatMap flagToString flagValues
    flagToString (flag, mValue) =
      let flagName = case shortName flag of
            Just sName -> "-" ++ sName
            Nothing -> "--" ++ longName flag
       in case (flagType flag, mValue) of
            (RequiresArg, Just val) -> [flagName, "\"" ++ val ++ "\""]
            (NoArg, _) -> [flagName]
            _ -> []

instance Arbitrary FlagType where
  arbitrary = elements [NoArg, RequiresArg]

instance Arbitrary Flag where
  arbitrary = do
    long <- listOf1 (elements ['a' .. 'z'])
    short <- frequency [(1, Just <$> listOf1 (elements ['a' .. 'z'])), (1, return Nothing)]
    Flag long short <$> arbitrary

instance Arbitrary Command where
  arbitrary = do
    subcmd <- listOf1 $ elements ['a' .. 'z']
    desc <- listOf $ elements $ ['a' .. 'z'] ++ [' ']
    numFlags <- chooseInt (0, 5)
    flags <- generateUniqueFlags numFlags
    return $ Command subcmd desc flags testValidate

generateUniqueFlags :: Int -> Gen [Flag]
generateUniqueFlags n = go n [] []
  where
    go 0 _ _ = return []
    go count usedLongNames usedShortNames = do
      long <- listOf1 (elements ['a' .. 'z'])
      if long `elem` usedLongNames
        then go count usedLongNames usedShortNames
        else do
          mShort <- frequency [(1, Just <$> listOf1 (elements ['a' .. 'z'])), (1, return Nothing)]
          let shortValid = case mShort of
                Just short -> short `notElem` usedShortNames && short /= long
                Nothing -> True
          if not shortValid
            then go count usedLongNames usedShortNames
            else do
              flagType <- arbitrary
              let newUsedLongNames = long : usedLongNames
              let newUsedShortNames = maybe usedShortNames (: usedShortNames) mShort
              rest <- go (count - 1) newUsedLongNames newUsedShortNames
              return $ Flag long mShort flagType : rest

-- Hash Module Tests

testHashModuleFunctions :: Test
testHashModuleFunctions =
  TestList
    [ TestCase $
        let originalText = "Symmetric compression and decompression" :: T.Text
            originalBS = textToByteString originalText
            compressed = compress originalBS
            decompressed = decompress compressed
            decoded = decompressed >>= byteStringToText
         in assertEqual "Compress and decompress Text should return original" (Right originalText) decoded,
      TestCase $ do
        let original = BS8.pack "The quick brown fox jumps over the lazy dog" :: BS.ByteString
            compressed = compress original
            decompressed = decompress compressed
        assertEqual "Decompressed ByteString should match original" (Right original) decompressed,
      TestCase $
        let input = BS8.pack "hello"
            expected = "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d" -- SHA-1 for "hello"
            actual = sha1Hash input
         in assertEqual "SHA-1 hash of 'hello'" expected actual,
      TestCase $
        let input = BS8.pack ""
            expected = "da39a3ee5e6b4b0d3255bfef95601890afd80709" -- SHA-1 for empty string
            actual = sha1Hash input
         in assertEqual "SHA-1 hash of empty ByteString" expected actual,
      TestCase $
        let input = BS8.pack "The quick brown fox jumps over the lazy dog"
            expected = "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12" -- SHA-1 for the given string
            actual = sha1Hash input
         in assertEqual "SHA-1 hash of pangram" expected actual
    ]

-- Git Init Tests

-- | Ensure the `git init` command creates the expected structure.
testGitInit :: Test
testGitInit = TestCase $ do
  currentDir <- getCurrentDirectory
  let testDir = currentDir </> "test-dir"
  doesDirectoryExist testDir >>= \exists -> when exists (removeDirectoryRecursive testDir)
  createDirectoryIfMissing' testDir
  setCurrentDirectory testDir
  let initCommand =
        ParsedCommand
          { parsedSubcommand = Command "init" "" [] defaultValidate,
            parsedFlags = [],
            parsedArguments = []
          }
  result <- commandHandler initCommand

  case result of
    Left (CommandError err) -> assertFailure $ "Command failed with error: " ++ err
    Right _ -> do
      let hgitPath = testDir </> ".hgit"
      exists <- doesDirectoryExist hgitPath
      assertBool ".hgit directory should exist" exists

      let objectsPath = hgitPath </> "objects"
      objectsExists <- doesDirectoryExist objectsPath
      assertBool "objects directory should exist" objectsExists

      let refsHeadsPath = hgitPath </> "refs" </> "heads"
      refsHeadsExists <- doesDirectoryExist refsHeadsPath
      assertBool "refs/heads directory should exist" refsHeadsExists

      let headFilePath = hgitPath </> "HEAD"
      headFileExists <- doesFileExist headFilePath
      assertBool "HEAD file should exist" headFileExists

  setCurrentDirectory currentDir
  removeDirectoryRecursive testDir

-- Git Add Tests

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
  let addCmd = head letCommands -- "add" command
  let parsedAddCmd =
        ParsedCommand
          { parsedSubcommand = addCmd,
            parsedFlags = [("update", Nothing)],
            parsedArguments = ["."]
          }
  resultAdd <- commandHandler parsedAddCmd
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
  resultAdd2 <- commandHandler parsedAddCmd2
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

-- | Tests for the `git add` command
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

-- Git Commit Tests

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
  runAddCommand [] ["file1.txt", "file2.txt"]
  runCommitCommand [("message", Just "Initial commit")] []

  -- Attempt to commit again without changes
  runCommitCommand [("message", Just "No changes commit")] []

  -- Get current commit OID
  headOid <- getHeadCommitOid testDir

  -- Verify commit
  verifyCommit testDir headOid (Just headOid) "No changes commit"

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

-- | Test committing object structure with subtrees
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

-- | Collection of all commit tests
testCommit :: Test
testCommit =
  TestLabel "Commit Command Tests" $
    TestList
      [ TestLabel "Commit Without Message" testCommitWithoutMessage,
        TestLabel "Commit With Empty Message" testCommitWithEmptyMessage,
        TestLabel "Initial Commit" testInitialCommit,
        TestLabel "Multiple Commits" testMultipleCommits,
        TestLabel "Commit Without Changes" testCommitNoChanges,
        TestLabel "Commit With Additional Files" testCommitWithAdditionalFiles,
        TestLabel "Commit Sequence" testCommitSequence,
        TestLabel "Commit Object Structure with Subtrees" testCommitObjectStructure
      ]

-- | Helper function to get the commit OID from HEAD
getHeadCommitOid :: FilePath -> IO String
getHeadCommitOid testDir = do
  -- Read the 'HEAD' file to get the ref path
  headRefBS <- readFileAsByteString (testDir </> ".hgit" </> "HEAD")
  let headRef = BS8.unpack $ BS8.strip headRefBS  -- e.g., "refs/heads/main"
  let refFilePath = testDir </> ".hgit" </> headRef
  -- Read the commit OID from the ref file
  headOidBS <- readFileAsByteString refFilePath
  return $ BS8.unpack $ BS8.strip headOidBS
-- Main Test Runner

main :: IO ()
main = do
  -- Run HUnit Tests
  _ <-
    runTestTT $
      TestList
        [ testParseCommand,
          testParseFlagsAndArgs,
          testParseInput,
          testHashModuleFunctions,
          testGitInit,
          testGitAdd,
          testCommit
        ]

  -- Run QuickCheck Properties
  quickCheckN 1000 prop_parseInput_correct

-- | Helper function to run QuickCheck with a specified number of tests
quickCheckN :: (Test.QuickCheck.Testable prop) => Int -> prop -> IO ()
quickCheckN n = quickCheckWith stdArgs {maxSuccess = n}