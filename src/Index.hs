{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Index
  ( readIndexFile,
    writeIndexFile,
    updateIndex,
    getAllFiles,
    getTrackedFiles,
  )
where

import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Bifunctor qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text.Encoding.Error (UnicodeException)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    listDirectory,
    removeDirectoryRecursive
  )
import System.FilePath
  ( makeRelative,
    normalise,
    splitDirectories,
    takeDirectory,
    takeFileName,
    (</>)
  )
import Data.List (isPrefixOf, (\\))
import CommandParser ( CommandError(..) )
import Utils
    ( doesDirectoryExist,
      createObject,
      getIndexFilePath,
      sha1Hash,
      stringToByteString,
      byteStringToString )

-- Type alias for the index mapping
type IndexMap = Map FilePath String -- Map from file path to OID

-- | Reads the index file into a Map
readIndexFile :: IO IndexMap
readIndexFile = do
  indexPath <- getIndexFilePath
  exists <- doesFileExist indexPath
  if not exists
    then return Map.empty
    else do
      content <- BS.readFile indexPath
      let linesOfFiles = BS8.lines content
      let entries = map parseIndexLine linesOfFiles
      return $ Map.fromList entries

-- | Parses a line from the index file into a (FilePath, OID) tuple
parseIndexLine :: ByteString -> (FilePath, String)
parseIndexLine line =
  let (oidBS, pathBS) = BS.break (== 32) line -- 32 is ASCII code for space
      oid = either (error "Invalid OID encoding") id $ byteStringToString oidBS
      path = either (error "Invalid path encoding") id $ byteStringToString (BS.drop 1 pathBS)
   in (path, oid)

-- | Writes the index Map back to the index file
writeIndexFile :: IndexMap -> IO ()
writeIndexFile indexMap = do
  indexPath <- getIndexFilePath
  let linesOfFiles = map (\(path, oid) -> stringToByteString $ oid ++ " " ++ path) $ Map.toList indexMap
  BS.writeFile indexPath (BS.intercalate (stringToByteString "\n") linesOfFiles)

-- | Updates the index Map with new or changed files
updateIndex :: IndexMap -> [FilePath] -> [(String, Maybe String)] -> IO (Either CommandError IndexMap)
updateIndex indexMap filepaths flags =
  case (flags, filepaths) of
    ([("update", Nothing)], []) -> updateTrackedFiles indexMap
    ([], []) -> do
      -- Equivalent to 'hgit add .', add all files
      allFiles <- getAllFiles
      updateFiles indexMap allFiles
    ([], _) -> updateFiles indexMap filepaths
    _ -> return $ Left $ CommandError "Invalid usage of 'hgit add'"

-- | Updates only the tracked files
updateTrackedFiles :: IndexMap -> IO (Either CommandError IndexMap)
updateTrackedFiles indexMap = do
  let trackedFiles = Map.keys indexMap
  updateFiles indexMap trackedFiles

-- | Updates the index with the specified files
updateFiles :: IndexMap -> [FilePath] -> IO (Either CommandError IndexMap)
updateFiles indexMap files = do
  results <- mapM (processFile indexMap) files
  case sequence results of
    Left err -> return $ Left err
    Right updatesList -> do
      let updates = concat updatesList -- Flatten the list
      let newIndexMap = foldl
            (\accMap (fp, oid) ->
               if oid == ""
                 then Map.delete fp accMap
                 else Map.insert fp oid accMap
            )
            indexMap
            updates
      return $ Right newIndexMap

-- | Processes a single file or directory and collects updates
processFile :: IndexMap -> FilePath -> IO (Either CommandError [(FilePath, String)])
processFile indexMap filepath = do
  isFile <- doesFileExist filepath
  isDir <- doesDirectoryExist filepath
  currentDir <- getCurrentDirectory
  let relativePath = makeRelative currentDir filepath
  let normalizedPath = normalise relativePath

  if isFile
    then do
      -- File exists: process for addition or update
      result <- processFileContents indexMap filepath
      case result of
        Left err -> return $ Left err
        Right maybeUpdate ->
          case maybeUpdate of
            Nothing -> return $ Right [] -- No changes
            Just update -> return $ Right [update] -- Single file update
    else if isDir
      then do
        -- Directory exists: recursively process all files within
        files <- getFilesRecursive filepath
        results <- mapM (processFile indexMap) files
        case sequence results of
          Left err -> return $ Left err
          Right updatesList -> return $ Right $ concat updatesList -- Collect updates
      else do
        -- Pathspec does not match any existing file or directory
        if Map.member normalizedPath indexMap
          then do
            -- The path was tracked as a file: remove it from the index
            return $ Right [(normalizedPath, "")] -- Empty OID signals removal
          else do
            -- Attempt to remove all files under the deleted directory from the index
            let updatedIndex = removeFilesInDirectory indexMap normalizedPath
                removedFiles = Map.keys indexMap \\ Map.keys updatedIndex -- Correctly compute removed files
            if null removedFiles
              then return $ Left $ CommandError $ "fatal: pathspec '" ++ filepath ++ "' did not match any files"
              else do
                -- Signal removal of multiple files
                let removalEntries = map (\fp -> (fp, "")) removedFiles
                return $ Right removalEntries

-- | Processes a single file's contents and determines if an update is needed
processFileContents :: IndexMap -> FilePath -> IO (Either CommandError (Maybe (FilePath, String)))
processFileContents indexMap filepath = do
  fileExists <- doesFileExist filepath
  if fileExists
    then do
      content <- BS.readFile filepath
      let oid = sha1Hash content
      -- Normalize the file path
      currentDir <- getCurrentDirectory
      let relativePath = makeRelative currentDir filepath
      let normalizedPath = normalise relativePath
      let existingOid = Map.lookup normalizedPath indexMap
      if existingOid == Just oid
        then return $ Right Nothing -- No changes needed
        else do
          createObject content
          return $ Right $ Just (normalizedPath, oid)
    else do
      -- File does not exist; check if it's tracked
      currentDir <- getCurrentDirectory
      let relativePath = makeRelative currentDir filepath
      let normalizedPath = normalise relativePath
      if Map.member normalizedPath indexMap
        then return $ Right $ Just (normalizedPath, "") -- Signal to remove from index
        else return $ Left $ CommandError $ "fatal: pathspec '" ++ filepath ++ "' did not match any files"

-- | Gets all files in the current directory recursively, excluding the .hgit directory
getAllFiles :: IO [FilePath]
getAllFiles = do
  currentDir <- getCurrentDirectory
  getFilesRecursive currentDir

-- | Helper function to get files recursively, excluding .hgit
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive dir = do
  contents <- listDirectory dir
  let filteredContents = filter (`notElem` [".", "..", ".hgit"]) contents
  let contentsFullPath = map (dir </>) filteredContents
  files <- filterM doesFileExist contentsFullPath
  dirs <- filterM doesDirectoryExist contentsFullPath
  nestedFiles <- forM dirs getFilesRecursive
  let allFiles = files ++ concat nestedFiles
  -- Normalize paths relative to current directory
  currentDir <- getCurrentDirectory
  return $ map (normalise . makeRelative currentDir) allFiles

-- | Gets all currently tracked files from the index
getTrackedFiles :: IO [FilePath]
getTrackedFiles = Map.keys <$> readIndexFile

-- | Helper function to check if a file is under a given directory
isUnderDirectory :: FilePath -> FilePath -> Bool
isUnderDirectory dirPath filePath =
  let normalizedDir = normalise (dirPath </> "")
      normalizedFile = normalise filePath
   in normalizedDir `isPrefixOf` normalizedFile

-- | Removes all files in the index that are under the specified directory
removeFilesInDirectory :: IndexMap -> FilePath -> IndexMap
removeFilesInDirectory indexMap dirPath =
  Map.filterWithKey (\path _ -> not (isUnderDirectory dirPath path)) indexMap