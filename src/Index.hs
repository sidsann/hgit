module Index
  ( readIndexFile,
    writeIndexFile,
    updateIndex,
    createBlobObject,
    getAllFiles,
    getTrackedFiles
  ) where

import CommandParser (CommandError(..))
import FileIO
    ( doesDirectoryExist,
      createDirectoryIfMissing',
      getObjectsPath,
      getIndexFilePath )
import Hash
import System.Directory (doesFileExist, listDirectory, getCurrentDirectory, doesDirectoryExist)
import System.FilePath ((</>), normalise, makeRelative)
import Control.Monad (filterM, forM, unless)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (catMaybes)
import Data.Text.Encoding.Error (UnicodeException)

-- Type alias for the index mapping
type IndexMap = Map FilePath String  -- Map from file path to OID

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
  let (oidBS, pathBS) = BS.break (== 32) line  -- 32 is ASCII code for space
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
  results <- mapM (processFile indexMap) files  -- Now returns list of updates
  case sequence results of
    Left err -> return $ Left err
    Right updatesList -> do
      let updates = concat updatesList  -- Flatten list of lists
      let newIndexMap = foldl (\accMap (fp, oid) -> Map.insert fp oid accMap) indexMap updates
      return $ Right newIndexMap

-- | Processes a single file or directory and collects updates
-- In Index.hs
processFile :: IndexMap -> FilePath -> IO (Either CommandError [(FilePath, String)])
processFile indexMap filepath = do
  isFile <- doesFileExist filepath
  isDir <- doesDirectoryExist filepath
  if isFile
    then do
      result <- processFileContents indexMap filepath
      case result of
        Left err -> return $ Left err
        Right maybeUpdate ->
          case maybeUpdate of
            Nothing -> return $ Right []  -- No changes
            Just update -> return $ Right [update]  -- Single file update
    else if isDir
      then do
        -- Recursively process all files in the directory
        files <- getFilesRecursive filepath
        results <- mapM (processFileContents indexMap) files
        case sequence results of
          Left err -> return $ Left err
          Right updates -> return $ Right $ catMaybes updates  -- Collect updates
      else return $ Left $ CommandError $ "fatal: pathspec '" ++ filepath ++ "' did not match any files"

processFileContents :: IndexMap -> FilePath -> IO (Either CommandError (Maybe (FilePath, String)))
processFileContents indexMap filepath = do
  content <- BS.readFile filepath
  let oid = sha1Hash content
  -- Normalize the file path
  currentDir <- getCurrentDirectory
  let relativePath = makeRelative currentDir filepath
  let normalizedPath = normalise relativePath
  let existingOid = Map.lookup normalizedPath indexMap
  if existingOid == Just oid
    then return $ Right Nothing  -- No changes needed
    else do
      createBlobObject oid content
      return $ Right $ Just (normalizedPath, oid)

-- | Creates a blob object in the .hgit/objects directory
createBlobObject :: String -> ByteString -> IO ()
createBlobObject oid content = do
  objectsPath <- getObjectsPath
  let (dirName, fileName) = splitAt 2 oid
  let dirPath = objectsPath </> dirName
  let filePath = dirPath </> fileName
  dirExists <- doesDirectoryExist dirPath
  unless dirExists $ createDirectoryIfMissing' dirPath
  fileExists <- doesFileExist filePath
  unless fileExists $ BS.writeFile filePath (compress content)

-- | Gets all files in the current directory recursively, excluding the .hgit directory
getAllFiles :: IO [FilePath]
getAllFiles = do
  currentDir <- getCurrentDirectory
  getFilesRecursive currentDir

-- | Helper function to get files recursively, excluding .hgit
-- In Index.hs

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive dir = do
  contents <- listDirectory dir
  let filteredContents = filter (`notElem` [".", "..", ".hgit"]) contents
  let contentsFullPath = map (dir </>) filteredContents
  files <- filterM doesFileExist contentsFullPath
  dirs <- filterM doesDirectoryExist contentsFullPath
  nestedFiles <- forM dirs getFilesRecursive
  currentDir <- getCurrentDirectory
  let allFiles = files ++ concat nestedFiles
  -- Normalize paths relative to current directory
  return $ map (normalise . makeRelative currentDir) allFiles

-- | Gets all currently tracked files from the index
getTrackedFiles :: IO [FilePath]
getTrackedFiles = Map.keys <$> readIndexFile