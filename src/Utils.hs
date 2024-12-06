{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( readFileAsByteString,
    writeFileFromByteString,
    createDirectoryIfMissing',
    createFileIfMissing,
    getHgitPath,
    getRefsPath,
    getHeadsPath,
    getObjectsPath,
    getHeadPath,
    getIndexFilePath,
    doesDirectoryExist,
    encodeUtf8,
    getHEADFilePath,
    createObject,
    readAndDecompressObject,
    compress,
    decompress,
    sha1Hash,
    byteStringToText,
    textToByteString,
    stringToByteString,
    byteStringToString,
    getHeadCommitOid
  )
where
import Control.Exception
    ( SomeException, try, catch, SomeException )
import Control.Monad ( filterM, forM, forM_, unless, when, unless )
import Data.Bifunctor qualified
import Data.ByteString.Char8 qualified as BS8
import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import System.Directory
    ( createDirectoryIfMissing,
      doesDirectoryExist,
      doesFileExist,
      getCurrentDirectory,
      listDirectory,
      removeDirectoryRecursive,
      setCurrentDirectory,
      doesDirectoryExist,
      doesFileExist,
      getCurrentDirectory,
      createDirectoryIfMissing )
import System.FilePath
    ( makeRelative,
      normalise,
      splitDirectories,
      takeDirectory,
      takeFileName,
      (</>),
      (</>) )
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
import Codec.Compression.Zlib qualified as Zlib
import Crypto.Hash.SHA1 qualified as SHA1
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Numeric (showHex)
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)

-- | FILE IO RELATED FUNCTIONS

-- | Creates an object in the .hgit/objects directory
createObject :: BS.ByteString -> IO String
createObject content = do
    -- Compute the OID as SHA-1 hash of the content
    let oid = sha1Hash content
        compressedContent = compress content
    -- Store the object
    objectsPath <- getObjectsPath
    let (dirName, fileName) = splitAt 2 oid
        dirPath = objectsPath </> dirName
        filePath = dirPath </> fileName
    createDirectoryIfMissing' dirPath
    -- Check if the object already exists 
    exists <- doesFileExist filePath
    unless exists $ do
        BS.writeFile filePath compressedContent
    return oid

-- | Helper function to read and decompress an object
readAndDecompressObject :: String -> IO (Either String BS.ByteString)
readAndDecompressObject oid = do
  let (dir, file) = splitAt 2 oid
  let objectPath = ".hgit/objects" </> dir </> file
  exists <- doesFileExist objectPath
  if not exists
    then return $ Left $ "Object does not exist: " ++ oid
    else do
      compressedContent <- BS.readFile objectPath
      case decompress compressedContent of
        Left err -> return $ Left err
        Right content -> return $ Right content

-- | Reads a file as a ByteString
readFileAsByteString :: FilePath -> IO BS.ByteString
readFileAsByteString = BS.readFile

-- | Writes a ByteString to a file
writeFileFromByteString :: FilePath -> BS.ByteString -> IO ()
writeFileFromByteString = BS.writeFile

-- | Creates a directory if it doesn't exist (recursively creates parent dirs if they don't exist)
-- Directly using System.Directory's createDirectoryIfMissing
createDirectoryIfMissing' :: FilePath -> IO ()
createDirectoryIfMissing' = System.Directory.createDirectoryIfMissing True

-- | Creates a file if it doesn't exist. If it exists, does nothing.
createFileIfMissing :: FilePath -> IO ()
createFileIfMissing path = do
  exists <- doesFileExist path
  if exists
    then return ()
    else writeFileFromByteString path BS.empty

-- | FILEPATH GETTERS

-- | Gets the path to the .hgit directory in the current working directory
getHgitPath :: IO FilePath
getHgitPath = do
  currentDir <- getCurrentDirectory
  return $ currentDir </> ".hgit"

-- | Gets the path to the refs directory within .hgit
getRefsPath :: IO FilePath
getRefsPath = do
  hgitPath <- getHgitPath
  return $ hgitPath </> "refs"

-- | Gets the path to the heads directory within refs
getHeadsPath :: IO FilePath
getHeadsPath = do
  refsPath <- getRefsPath
  return $ refsPath </> "heads"

-- | Gets the path to the objects directory within .hgit
getObjectsPath :: IO FilePath
getObjectsPath = do
  hgitPath <- getHgitPath
  return $ hgitPath </> "objects"

-- | Constructs the full path to a branch within refs/heads
getHeadPath :: String -> IO FilePath
getHeadPath branchName = do
    headsPath <- getHeadsPath
    return $ headsPath </> branchName

-- | Gets the path to the index file within .hgit
getIndexFilePath :: IO FilePath
getIndexFilePath = do
  hgitPath <- getHgitPath
  return $ hgitPath </> "index"

getHEADFilePath :: IO FilePath
getHEADFilePath = do
  hgitPath <- getHgitPath
  return $ hgitPath </> "HEAD"

-- | ENCODING/DECODING RELATED FUNCTIONS

-- Compresses a strict ByteString using zlib
compress :: BS.ByteString -> BS.ByteString
compress = BL.toStrict . Zlib.compress . BL.fromStrict

-- Decompresses a strict ByteString using zlib
decompress :: BS.ByteString -> Either String BS.ByteString
decompress bs =
  case safeDecompress bs of
    Left err -> Left $ "Decompression error: " ++ err
    Right result -> Right $ BL.toStrict result
  where
    safeDecompress input = Right (Zlib.decompress $ BL.fromStrict input)

-- | Pads a single hex digit with a leading zero if necessary
padZero :: String -> String
padZero s = if length s == 1 then '0' : s else s

-- | Computes the SHA-1 hash of a strict ByteString and returns it as a hexadecimal String
sha1Hash :: BS.ByteString -> String
sha1Hash bs = concatMap (padZero . (`showHex` "")) (BS.unpack $ SHA1.hash bs)

-- Converts a ByteString to Text using UTF-8 decoding with error handling
byteStringToText :: BS.ByteString -> Either String T.Text
byteStringToText bs =
  case TE.decodeUtf8' bs of
    Right text -> Right text
    Left err -> Left $ "Text decoding error: " ++ show err

-- alias for TE.encodeUtf8
textToByteString :: T.Text -> BS.ByteString
textToByteString = TE.encodeUtf8

-- string to text and text to bytestring
stringToByteString :: String -> BS.ByteString
stringToByteString = TE.encodeUtf8.Data.Text.pack

byteStringToString :: BS.ByteString -> Either UnicodeException String
byteStringToString bs =   T.unpack <$> TE.decodeUtf8' bs

-- Helper function to get the commit OID from HEAD
getHeadCommitOid :: FilePath -> IO String
getHeadCommitOid testDir = do
  -- Read the 'HEAD' file to get the ref path
  headRefBS <- readFileAsByteString (testDir </> ".hgit" </> "HEAD")
  let headRef = BS8.unpack $ BS8.strip headRefBS -- e.g., "refs/heads/main"
  let refFilePath = testDir </> ".hgit" </> headRef
  -- Read the commit OID from the ref file
  headOidBS <- readFileAsByteString refFilePath
  return $ BS8.unpack $ BS8.strip headOidBS
