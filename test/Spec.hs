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
import Control.Monad (forM_, when)
import Data.Bifunctor qualified
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text qualified as T
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
  )
import Hash (byteStringToText, compress, decompress, sha1Hash, textToByteString)
import Index (readIndexFile)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    removeDirectoryRecursive,
    removePathForcibly,
    setCurrentDirectory,
  )
import System.FilePath (normalise, takeDirectory, (</>))
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
    vectorOf,
    (==>),
  )
import Test.QuickCheck.Property qualified

-- | Validation function that allows any flags and arguments
testValidate :: [(String, Maybe String)] -> [String] -> Either CommandError ()
testValidate _ _ = Right ()

letFlagsAdd :: [Flag]
letFlagsAdd =
  [ Flag "update" (Just "u") NoArg
  ]

letFlagsInit :: [Flag]
letFlagsInit = []

letCommands :: [Command]
letCommands =
  [ Command "add" "Add files to the index" letFlagsAdd testValidate,
    Command "init" "Initialize a new repository" letFlagsInit testValidate
  ]

-- | Helper function to create multiple files with specified content
createFiles :: [(FilePath, String)] -> IO ()
createFiles = mapM_ (\(f, content) -> createDirectoryIfMissing True (takeDirectory f) >> writeFile f content)

-- | Runs the 'git add' command with given flags and arguments
runAddCommand :: [(String, Maybe String)] -> [String] -> IO ()
runAddCommand flags args = do
  let addCmd = head letCommands -- "add" command
  let parsedAddCmd =
        ParsedCommand
          { parsedSubcommand = addCmd,
            parsedFlags = flags,
            parsedArguments = args
          }
  resultAdd <- commandHandler parsedAddCmd
  case resultAdd of
    Left (CommandError err) -> assertFailure $ "Add command failed: " ++ err
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
verifyBlobExists :: FilePath -> String -> String -> IO ()
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
  let initCmd = letCommands !! 1 -- "init" command
  let initParsedCmd =
        ParsedCommand
          { parsedSubcommand = initCmd,
            parsedFlags = [],
            parsedArguments = []
          }
  resultInit <- commandHandler initParsedCmd
  case resultInit of
    Left (CommandError err) -> do
      setCurrentDirectory originalDir
      removeDirectoryRecursive testDir
      assertFailure $ "Initialization failed: " ++ err
    Right _ -> do
      action testDir
      setCurrentDirectory originalDir
      removeDirectoryRecursive testDir

testParseCommand :: Test
testParseCommand =
  TestList
    [ "Valid Command 'add'"
        ~: parseCommand letCommands ["add"]
        ~?= Right (head letCommands, []),
      "Valid Command 'init'"
        ~: parseCommand letCommands ["init"]
        ~?= Right (letCommands !! 1, []),
      "Invalid Command"
        ~: parseCommand letCommands ["status"]
        ~?= Left (CommandError "Unknown command: status")
    ]

testParseFlagsAndArgs :: Test
testParseFlagsAndArgs =
  TestList
    [ "Parse no-arg flag (--update)"
        ~: parseFlagsAndArgs letFlagsAdd ["--update"]
        ~?= Right ([("update", Nothing)], []),
      "Parse short flag (-u)"
        ~: parseFlagsAndArgs letFlagsAdd ["-u"]
        ~?= Right ([("update", Nothing)], []),
      "Parse arguments"
        ~: parseFlagsAndArgs letFlagsAdd ["file1.txt", "file2.txt"]
        ~?= Right ([], ["file1.txt", "file2.txt"]),
      "Parse flags and arguments"
        ~: parseFlagsAndArgs letFlagsAdd ["--update", "file1.txt"]
        ~?= Right ([("update", Nothing)], ["file1.txt"])
    ]

testParseInput :: Test
testParseInput =
  TestList
    [ "Parse 'add' with no flags or args"
        ~: parseInput letCommands "add"
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = head letCommands,
                parsedFlags = [],
                parsedArguments = []
              }
          ),
      "Parse 'add' with --update flag"
        ~: parseInput letCommands "add --update"
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = head letCommands,
                parsedFlags = [("update", Nothing)],
                parsedArguments = []
              }
          ),
      "Parse 'add' with files"
        ~: parseInput letCommands "add file1.txt file2.txt"
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = head letCommands,
                parsedFlags = [],
                parsedArguments = ["file1.txt", "file2.txt"]
              }
          ),
      "Parse 'add' with flags and files"
        ~: parseInput letCommands "add -u file1.txt"
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = head letCommands,
                parsedFlags = [("update", Nothing)],
                parsedArguments = ["file1.txt"]
              }
          ),
      "Parse 'init' with no flags or args"
        ~: parseInput letCommands "init"
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = letCommands !! 1,
                parsedFlags = [],
                parsedArguments = []
              }
          ),
      "Parse 'init' with unexpected args"
        ~: parseInput letCommands "init extra"
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = letCommands !! 1,
                parsedFlags = [],
                parsedArguments = ["extra"]
              }
          )
    ]

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

-- | Hash module tests
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

-- | Main Test Runner
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
          testGitAdd
        ]

  -- Run QuickCheck Properties
  quickCheckN 1000 prop_parseInput_correct

-- | Helper function to run QuickCheck with a specified number of tests
quickCheckN :: (Test.QuickCheck.Testable prop) => Int -> prop -> IO ()
quickCheckN n = quickCheckWith stdArgs {maxSuccess = n}