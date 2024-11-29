-- FileIO.hs
module FileIO
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
    getHEADFilePath
  )
where

import qualified Data.ByteString as BS
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory, createDirectoryIfMissing
  )
import System.FilePath ( (</>) )
import Control.Exception (catch, SomeException)
import Data.Text.Encoding (encodeUtf8)

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
