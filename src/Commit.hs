module Commit where

import CommandParser (CommandError (..))
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import FileIO ( getHgitPath, getHEADFilePath, createObject )
import Hash ( stringToByteString )
import Index (readIndexFile)
import System.Directory (doesFileExist)
import System.FilePath
    ( (</>),
      takeDirectory,
      takeFileName,
      (</>), splitDirectories )
import qualified Data.ByteString.Char8 as BS8
import Data.List (sort, isPrefixOf)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

buildTree :: Map FilePath String -> IO String
buildTree = buildTreeRecursive ""

buildTreeRecursive :: FilePath -> Map FilePath String -> IO String
buildTreeRecursive dirPath indexMap = do
    -- Separate entries into blobs and subtrees
    let entriesInDir = Map.filterWithKey (\k _ -> takeDirectory k == dirPath) indexMap
    let entriesInSubDirs = Map.filterWithKey (\k _ -> isSubdirectory dirPath k) indexMap

    -- Process blobs (files in the current directory)
    let blobEntries = Map.toList entriesInDir

    -- Process subtrees
    let subDirs = collectSubDirs entriesInSubDirs
    subTreeOids <- Map.traverseWithKey (\subDir _ -> buildTreeRecursive subDir indexMap) subDirs

    -- Build tree entries
    let blobTreeEntries = map (\(path, oid) -> buildTreeEntry "blob" oid (takeFileName path)) blobEntries
    let treeTreeEntries = map (\(dir, oid) -> buildTreeEntry "tree" oid (takeFileName dir)) $ Map.toList subTreeOids

    let treeContent = BS.concat $ sort $ blobTreeEntries ++ treeTreeEntries

    -- Create tree object
    createObject treeContent

-- Helper function to collect unique subdirectories
collectSubDirs :: Map FilePath a -> Map FilePath ()
collectSubDirs = Map.fromList . map (\k -> (takeDirectory k, ())) . Map.keys

buildTreeEntry :: String -> String -> String -> BS.ByteString
buildTreeEntry objType oid name =
    let entryLine = objType ++ " " ++ oid ++ " " ++ name ++ "\n"
    in BS8.pack entryLine

-- Check if a path is a subdirectory of another
isSubdirectory :: FilePath -> FilePath -> Bool
isSubdirectory dir path =
    let dirComponents = splitPathComponents dir
        pathComponents = splitPathComponents (takeDirectory path)
    in dirComponents `isPrefixOf` pathComponents && dirComponents /= pathComponents

-- Split a path into components
splitPathComponents :: FilePath -> [FilePath]
splitPathComponents = splitDirectories

createCommitContent :: String -> Maybe String -> String -> String -> BS.ByteString
createCommitContent treeOid parentOid timestamp commitMsg =
    let parentLine = maybe "" (\oid -> "parent " ++ oid ++ "\n") parentOid
        content = "tree " ++ treeOid ++ "\n"
                  ++ parentLine
                  ++ "timestamp " ++ timestamp ++ " +0000\n"
                  ++ commitMsg ++ "\n"
    in BS8.pack content

-- | Gets the current commit OID from HEAD
getCurrentCommitOid :: IO (Maybe String)
getCurrentCommitOid = do
  headPath <- getHEADFilePath
  ref <- BS.readFile headPath
  let refPath = BS8.unpack ref -- e.g., "refs/heads/main"
  refFullPath <- fmap (</> refPath) getHgitPath
  exists <- doesFileExist refFullPath
  if exists
    then do
      oid <- BS.readFile refFullPath
      let oidStr = BS8.unpack oid
      if null oidStr
        then return Nothing -- No parent commit
        else return $ Just oidStr
    else return Nothing -- No parent commit

-- | Updates the HEAD reference to point to the new commit
updateHEAD :: String -> IO ()
updateHEAD commitOid = do
  headPath <- getHEADFilePath
  ref <- BS.readFile headPath
  let refPath = BS8.unpack ref -- e.g., "refs/heads/main"
  refFullPath <- fmap (</> refPath) getHgitPath
  BS.writeFile refFullPath (stringToByteString commitOid)