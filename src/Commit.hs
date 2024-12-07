{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Commit where

import CommandParser (CommandError (..))
import Control.Monad (forM_, unless, when, forM)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (isPrefixOf, sort, intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Index (readIndexFile, writeIndexFile, getAllFiles)
import System.Directory (doesFileExist, removeFile)
import System.FilePath
  ( splitDirectories,
    takeDirectory,
    takeFileName,
    (</>),
  )
import Utils
  ( createObject,
    getHEADFilePath,
    getHgitPath,
    readAndDecompressObject,
    stringToByteString, createDirectoryIfMissing', sha1Hash,
  )
import Data.Time.LocalTime (TimeZone, utcToLocalTime, getCurrentTimeZone)
import Control.Exception (throwIO)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | Data structure representing a Commit
data Commit = Commit
  { treeOid :: String,
    parentOid :: Maybe String,
    timestamp :: String,
    message :: String
  }
  deriving (Show, Eq)

-- | Data structure representing a Tree
data Tree = Tree
  { treeEntries :: [(String, String, String)] -- (Type, OID, Name)
  }
  deriving (Show, Eq)

-- | Helper function to deserialize a commit object
deserializeCommit :: String -> IO (Either String Commit)
deserializeCommit oid = do
  objResult <- readAndDecompressObject oid
  case objResult of
    Left err -> return $ Left err
    Right content -> parseCommitContent content

-- | Helper function to parse commit content
parseCommitContent :: BS.ByteString -> IO (Either String Commit)
parseCommitContent content = do
  let linesOfContent = BS8.lines content
  let parseLine line =
        let (key, rest) = BS8.break (== ' ') line
            value = BS8.unpack $ BS8.drop 1 rest
         in (BS8.unpack key, value)
  let parsed = map parseLine linesOfContent
  let commitMap = Map.fromList parsed
  let treeOidVal = Map.lookup "tree" commitMap
  let parentOidVal = Map.lookup "parent" commitMap
  let timestampVal = Map.lookup "timestamp" commitMap
  -- Assume commit message is the last line after a blank line
  let messageVal = case dropWhile (/= BS.empty) (BS8.lines content) of
        [] -> Nothing
        (_ : msgLines) -> Just $ BS8.unpack $ BS8.intercalate (BS8.pack "\n") msgLines
  case (treeOidVal, messageVal) of
    (Just treeOid, Just message) ->
      return $
        Right
          Commit
            { treeOid = treeOid,
              parentOid = parentOidVal,
              timestamp = fromMaybe "" timestampVal,
              message = message
            }
    _ -> return $ Left "Invalid commit object format."

-- | Deserialize a tree object from its OID
deserializeTree :: String -> IO (Either String Tree)
deserializeTree oid = do
  objResult <- readAndDecompressObject oid
  case objResult of
    Left err -> return $ Left $ "Tree object not found: " ++ err
    Right content -> parseTree content

-- | Parse tree content into Tree data structure
parseTree :: BS.ByteString -> IO (Either String Tree)
parseTree content = do
  let linesOfContent = BS8.lines content
  let entries = map parseTreeEntry linesOfContent
  return $ Right $ Tree entries

-- | Parse a single tree entry line
-- Format: "type oid name"
parseTreeEntry :: BS.ByteString -> (String, String, String)
parseTreeEntry line =
  let (typeBS, rest1) = BS8.break (== ' ') line
      (oidBS, rest2) = BS8.break (== ' ') (BS8.drop 1 rest1)
      nameBS = BS8.drop 1 rest2
   in (BS8.unpack typeBS, BS8.unpack oidBS, BS8.unpack nameBS)

-- | Build tree OID from index
buildTree :: Map FilePath String -> IO String
buildTree = buildTreeRecursive "."

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

  -- Create tree object and return its OID
  createObject treeContent

-- | Collect unique subdirectories, excluding the current directory
collectSubDirs :: Map FilePath a -> Map FilePath ()
collectSubDirs =
  Map.fromList
    . map (\k -> (takeDirectory k, ()))
    . filter (\k -> takeDirectory k /= ".")
    . Map.keys

-- | Create a tree entry
buildTreeEntry :: String -> String -> String -> BS.ByteString
buildTreeEntry objType oid name =
  let entryLine = objType ++ " " ++ oid ++ " " ++ name ++ "\n"
   in BS8.pack entryLine

-- | Check if a path is a subdirectory of another
isSubdirectory :: FilePath -> FilePath -> Bool
isSubdirectory dirPath path =
  let dirComponents = if dirPath == "." then [] else splitPathComponents dirPath
      pathDir = takeDirectory path
      pathComponents = if pathDir == "." then [] else splitPathComponents pathDir
   in dirComponents `isPrefixOf` pathComponents && dirComponents /= pathComponents

-- | Split a path into components
splitPathComponents :: FilePath -> [FilePath]
splitPathComponents = splitDirectories

-- | Create commit content
createCommitContent :: String -> Maybe String -> String -> String -> BS.ByteString
createCommitContent treeOid parentOid timestamp commitMsg =
  let parentLine = maybe "" (\oid -> "parent " ++ oid ++ "\n") parentOid
      content =
        "tree "
          ++ treeOid
          ++ "\n"
          ++ parentLine
          ++ "timestamp "
          ++ timestamp
          ++ "\n\n"
          ++ commitMsg
   in BS8.pack content

-- | Get the current commit OID from HEAD
getCurrentCommitOid :: IO (Maybe String)
getCurrentCommitOid = do
  headPath <- getHEADFilePath
  ref <- BS.readFile headPath
  let refPath = BS8.unpack $ BS8.strip ref -- e.g., "refs/heads/main"
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

-- | Update the HEAD reference to point to the new commit
updateHEAD :: String -> IO ()
updateHEAD commitOid = do
  headPath <- getHEADFilePath
  ref <- BS.readFile headPath
  let refPath = BS8.unpack ref -- e.g., "refs/heads/main"
  refFullPath <- fmap (</> refPath) getHgitPath
  BS.writeFile refFullPath (stringToByteString commitOid)

-- | Traverse commits starting from the given OID
traverseCommits :: String -> [Commit] -> IO String
traverseCommits oid acc = do
  commitResult <- deserializeCommit oid
  case commitResult of
    Left err -> return $ "Error reading commit " ++ oid ++ ": " ++ err
    Right commit -> do
      let newAcc = acc ++ [commit]
      case parentOid commit of
        Nothing -> formatCommits newAcc
        Just parent -> traverseCommits parent newAcc

formatCommits :: [Commit] -> IO String
formatCommits commits = do
  tz <- getCurrentTimeZone
  let reversedCommits = reverse commits
      formatted = case reversedCommits of
        [] -> []
        (latest:rest) ->
          formatCommitWithTZ tz latest True :
          map (formatCommitWithTZ tz `flip` False) rest
  return $ intercalate "\n\n" formatted

formatCommitWithTZ :: TimeZone -> Commit -> Bool -> String
formatCommitWithTZ tz commit isHead =
  let epochStr = timestamp commit
      epochTime = read epochStr :: Int
      utcTime = posixSecondsToUTCTime (fromIntegral epochTime)
      localTime = utcToLocalTime tz utcTime
      dateString = formatTime defaultTimeLocale "%a %b %e %T %Y %z" localTime
      headIndicator = if isHead then " (HEAD -> main)" else ""
  in "commit " ++ treeOid commit ++ headIndicator ++ "\n" ++
     "Date:   " ++ dateString ++ "\n\n" ++
     "    " ++ message commit

checkoutTree :: Tree -> FilePath -> IO (Map FilePath String)
checkoutTree (Tree entries) dir = do
  indexMaps <- forM entries $ \(typ, oid, name) -> do
    let path = if dir == "." then name else dir </> name
    case typ of
      "blob" -> do
        contentResult <- readAndDecompressObject oid
        case contentResult of
          Left err -> throwIO $ CommandError $ "Failed to read blob object: " ++ err
          Right content -> do
            createDirectoryIfMissing' (takeDirectory path)
            BS.writeFile path content
            return $ Map.singleton path oid
      "tree" -> do
        treeResult <- deserializeTree oid
        case treeResult of
          Left err -> throwIO $ userError $ "Failed to deserialize subtree: " ++ err
          Right subtree -> checkoutTree subtree path
      _ -> throwIO $ userError $ "Unknown object type in tree: " ++ typ
  return (Map.unions indexMaps)

anyModifiedFile :: Map FilePath String -> IO Bool
anyModifiedFile indexMap = do
  or <$> mapM fileChanged (Map.toList indexMap)
  where
    fileChanged (path, oid) = do
      exists <- doesFileExist path
      if not exists
        then return True -- file deleted => change
        else do
          content <- BS.readFile path
          let currentOid = sha1Hash content
          return (currentOid /= oid)

checkoutCommit :: String -> IO ()
checkoutCommit commitOid = do
  commitResult <- deserializeCommit commitOid
  case commitResult of
    Left err -> throwIO $ userError $ "Failed to deserialize commit: " ++ err
    Right commit -> do
      treeResult <- deserializeTree (treeOid commit)
      case treeResult of
        Left err -> throwIO $ userError $ "Failed to deserialize tree: " ++ err
        Right tree -> do
          clearWorkingDirectory
          indexMap <- checkoutTree tree "."
          writeIndexFile indexMap

clearWorkingDirectory :: IO ()
clearWorkingDirectory = do
  allFiles <- getAllFiles
  forM_ allFiles $ \f -> do
    -- Only skip removing if path starts with ".hgit"
    unless (".hgit" `isPrefixOf` f) $ removeFileIfExists f
  where
    removeFileIfExists fp = do
      e <- doesFileExist fp
      when e (removeFile fp)

uncommittedChangesExist :: IO Bool
uncommittedChangesExist = do
  indexMap <- readIndexFile
  allFiles <- getAllFiles

  let trackedFiles = Map.keys indexMap
      untracked = filter (`notElem` trackedFiles) allFiles

  if not (null untracked)
    then return True
    else do
      anyModifiedFile indexMap
