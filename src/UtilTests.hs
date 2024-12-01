{-# LANGUAGE OverloadedStrings #-}

module UtilTests
  ( utilTests,
  )
where
import Utils
    ( compress,
      decompress,
      sha1Hash,
      byteStringToText,
      textToByteString )
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Test.HUnit ( assertEqual, Test(..) )

utilTests :: Test
utilTests =
  TestList
    [ TestCase $
        let originalText = "Symmetric compression and decompression" :: T.Text
            originalBS = textToByteString originalText
            compressed = compress originalBS
            decompressed = decompress compressed
            decoded = decompressed >>= byteStringToText
         in assertEqual "Compress and decompress Text should return original" (Right originalText) decoded,
      TestCase $ do
        let original = BS8.pack "The quick brown fox jumps over the lazy dog" :: BS8.ByteString
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
