{-# LANGUAGE OverloadedStrings #-}

module Parser where

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

parseTests = TestList [testParseCommand,
             testParseFlagsAndArgs,
             testParseInput]



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