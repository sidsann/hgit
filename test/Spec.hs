{-# LANGUAGE OverloadedStrings #-}

import CommandHandler
import CommandParser
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Set qualified as Set
import Data.Text qualified as T
import Hash (compress, decompress, sha1Hash, byteStringToText, textToByteString)
import Test.HUnit
import Test.QuickCheck

letFlags :: [Flag]
letFlags =
  [ Flag "--message" (Just "-m") Required,
    Flag "--verbose" (Just "-v") Optional
  ]

letCommands :: [Command]
letCommands =
  [Command "commit" "Commit changes" letFlags []]

testParseCommand :: Test
testParseCommand =
  TestList
    [ "Valid Command"
        ~: parseCommand letCommands ["commit"]
        ~?= Right (head letCommands, []),
      "Invalid Command"
        ~: parseCommand letCommands ["status"]
        ~?= Left (CommandError "Unknown command: status")
    ]

testParseFlagsAndArgs :: Test
testParseFlagsAndArgs =
  TestList
    [ "Parse valid required flag (--message)"
        ~: parseFlagsAndArgs letFlags ["--message", "Commit message"]
        ~?= Right ([("--message", Just "Commit message")], []),
      "Parse valid short flag (-m)"
        ~: parseFlagsAndArgs letFlags ["-m", "Commit message"]
        ~?= Right ([("--message", Just "Commit message")], []),
      "Missing required flag value (--message)"
        ~: parseFlagsAndArgs letFlags ["--message"]
        ~?= Left (CommandError "Flag --message requires a value, but none was provided."),
      "Parse optional flag (--verbose)"
        ~: parseFlagsAndArgs letFlags ["--verbose"]
        ~?= Right ([("--verbose", Nothing)], []),
      "Parse both required and optional flags"
        ~: parseFlagsAndArgs letFlags ["--message", "Commit message", "--verbose"]
        ~?= Right ([("--message", Just "Commit message"), ("--verbose", Nothing)], []),
      "Conflicting flags (--message and -m)"
        ~: parseInput letCommands "commit -m \"Commit message\" --message \"Another message\""
        ~?= Left (CommandError "Conflicting flags used: fromList [\"--message\",\"-m\"]")
    ]

testParseInput :: Test
testParseInput =
  TestList
    [ "Parse valid input with required flag (--message)"
        ~: parseInput letCommands "commit --message \"Commit message\""
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = head letCommands,
                parsedFlags = [("--message", Just "Commit message")],
                parsedArguments = []
              }
          ),
      "Parse valid input with short flag (-m)"
        ~: parseInput letCommands "commit -m \"Commit message\""
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = head letCommands,
                parsedFlags = [("--message", Just "Commit message")],
                parsedArguments = []
              }
          ),
      "Missing required flag (--message)"
        ~: parseInput letCommands "commit --verbose"
        ~?= Left (CommandError "Flag --message requires a value, but none was provided."),
      "Conflicting flags (--message and -m)"
        ~: parseInput letCommands "commit -m \"Commit message\" --message \"Another message\""
        ~?= Left (CommandError "Conflicting flags used: fromList [\"--message\",\"-m\"]"),
      "Extra arguments after flags"
        ~: parseInput letCommands "commit --message \"Commit message\" extra args here"
        ~?= Right
          ( ParsedCommand
              { parsedSubcommand = head letCommands,
                parsedFlags = [("--message", Just "Commit message")],
                parsedArguments = ["extra", "args", "here"]
              }
          )
    ]

instance Arbitrary FlagType where
  arbitrary = elements [Optional, Required]

instance Arbitrary Flag where
  arbitrary = do
    long <- ("--" ++) <$> listOf1 (elements ['a' .. 'z'])
    short <- frequency [(1, Just . ('-' :) <$> listOf1 (elements ['a' .. 'z'])), (1, return Nothing)]
    Flag long short <$> arbitrary

instance Arbitrary Command where
  arbitrary = do
    subcmd <- listOf1 $ elements ['a' .. 'z']
    desc <- listOf $ elements $ ['a' .. 'z'] ++ [' ']
    numFlags <- chooseInt (0, 5)
    flags <- vectorOf numFlags arbitrary
    argsList <- listOf $ listOf1 $ elements ['a' .. 'z']
    return $ Command subcmd desc (uniqueFlags flags) argsList
    where
      uniqueFlags :: [Flag] -> [Flag]
      uniqueFlags fs = Set.toList $ Set.fromList fs

constructInputString :: Command -> [(Flag, Maybe String)] -> [String] -> String
constructInputString cmd flagValues argsList =
  unwords $ [subcommand cmd] ++ flagStrings ++ argsList
  where
    flagStrings = concatMap flagToString flagValues
    flagToString (flag, mValue) =
      let flagName = case shortName flag of
            Just sName -> sName
            Nothing -> longName flag
       in case (flagType flag, mValue) of
            (Required, Just val) -> [flagName, "\"" ++ val ++ "\""]
            (Optional, _) -> [flagName]
            _ -> []

prop_parseInput_correct :: Property
prop_parseInput_correct = forAll arbitrary $ \cmd ->
  let flagsWithValues = map assignFlagValue (flags cmd)
      cmdArgs = args cmd
      inputString = constructInputString cmd flagsWithValues cmdArgs
      expectedParsedFlags = map (\(flag, mVal) -> (longName flag, mVal)) flagsWithValues
      expectedParsedCommand =
        ParsedCommand
          { parsedSubcommand = cmd,
            parsedFlags = expectedParsedFlags,
            parsedArguments = cmdArgs
          }
   in case parseInput [cmd] inputString of
        Right parsedCmd -> parsedCmd == expectedParsedCommand
        Left _ -> False
  where
    assignFlagValue flag = case flagType flag of
      Required -> (flag, Just "value")
      Optional -> (flag, Nothing)

testCompressDecompress :: Test
testCompressDecompress = TestCase $ do
  let original = BS8.pack "The quick brown fox jumps over the lazy dog" :: BS.ByteString
      compressed = compress original
      decompressed = decompress compressed
  assertEqual "Decompressed ByteString should match original" (Right original) decompressed

-- | Test the symmetry of compress and decompress with Text
testCompressDecompressText :: Test
testCompressDecompressText =
  TestList
    [ TestCase $
        let originalText = "Symmetric compression and decompression" :: T.Text
            originalBS = textToByteString originalText
            compressed = compress originalBS
            decompressed = decompress compressed
            decoded = decompressed >>= byteStringToText
         in assertEqual "Compress and decompress Text should return original" (Right originalText) decoded
    ]

-- | Test the SHA-1 hash against known values
testSha1Hash :: Test
testSha1Hash =
  TestList
    [ TestCase $
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

-- Main Test Runner
main :: IO ()
main = do
  _ <- runTestTT $ TestList [testParseCommand, testParseFlagsAndArgs, testParseInput, testCompressDecompress, testSha1Hash, testCompressDecompressText]
  quickCheck prop_parseInput_correct