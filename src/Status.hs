module Status
  ( getCurrentBranchName,
    getHEADTreeMap,
    buildWorkingDirectoryMap,
    computeChangesToBeCommitted,
    computeChangesNotStaged,
    computeUntrackedFiles,
    formatStatusOutput
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import System.FilePath (takeFileName, (</>))
import Control.Monad (forM)
import Utils (getHEADFilePath, sha1Hash)
import Commit (deserializeCommit, deserializeTree, getCurrentCommitOid, Commit(..), Tree(..))
import Index (readIndexFile)

getCurrentBranchName :: IO String
getCurrentBranchName = do
  headPath <- getHEADFilePath
  ref <- BS.readFile headPath
  let refPath = BS8.unpack $ BS8.strip ref
  return $ takeFileName refPath

getHEADTreeMap :: IO (Map FilePath String)
getHEADTreeMap = do
  commitOidM <- getCurrentCommitOid
  case commitOidM of
    Nothing -> return Map.empty
    Just commitOid -> do
      commitResult <- deserializeCommit commitOid
      case commitResult of
        Left _err -> return Map.empty
        Right commit -> do
          treeResult <- deserializeTree (treeOid commit)
          case treeResult of
            Left _err -> return Map.empty
            Right tree -> collectBlobsFromTree "." tree

-- Recursively collect all blobs from the tree and its subtrees
collectBlobsFromTree :: FilePath -> Tree -> IO (Map FilePath String)
collectBlobsFromTree basePath (Tree entries) = do
  fmap Map.unions $ forM entries $ \(typ, oid, name) -> case typ of
    "blob" ->
      -- Add this blob to the map
      return $ Map.singleton (if basePath == "." then name else basePath </> name) oid
    "tree" -> do
      -- Recursively fetch blobs from the subtree
      subtreeResult <- deserializeTree oid
      case subtreeResult of
        Left _ -> return Map.empty
        Right subtree ->
          collectBlobsFromTree (if basePath == "." then name else basePath </> name) subtree
    _ -> return Map.empty

buildWorkingDirectoryMap :: [FilePath] -> IO (Map FilePath String)
buildWorkingDirectoryMap files = do
  fmap Map.fromList $ forM files $ \f -> do
    content <- BS.readFile f
    let oid = sha1Hash content
    return (f, oid)

-- Compute "Changes to be committed"
-- Compare HEAD vs Index
computeChangesToBeCommitted :: Map FilePath String -> Map FilePath String -> [(String, FilePath)]
computeChangesToBeCommitted headMap indexMap =
  let allPaths = Map.keys headMap ++ Map.keys indexMap
      uniquePaths = Set.toList $ Set.fromList allPaths
  in mapMaybe (classifyChangeToCommit headMap indexMap) uniquePaths

classifyChangeToCommit :: Map FilePath String -> Map FilePath String -> FilePath -> Maybe (String, FilePath)
classifyChangeToCommit headMap indexMap path =
  case (Map.lookup path headMap, Map.lookup path indexMap) of
    (Nothing, Just _idxOid) -> Just ("new file", path)
    (Just _headOid, Nothing) -> Just ("deleted", path)
    (Just headOid, Just idxOid) ->
      if headOid /= idxOid then Just ("modified", path) else Nothing
    (Nothing, Nothing) -> Nothing

-- Compute "Changes not staged for commit"
-- Compare Index vs Working Directory
computeChangesNotStaged :: Map FilePath String -> Map FilePath String -> [(String, FilePath)]
computeChangesNotStaged indexMap workingMap =
  let allPaths = Map.keys indexMap
  in mapMaybe (classifyNotStagedChange indexMap workingMap) allPaths

classifyNotStagedChange :: Map FilePath String -> Map FilePath String -> FilePath -> Maybe (String, FilePath)
classifyNotStagedChange indexMap workingMap path =
  case (Map.lookup path indexMap, Map.lookup path workingMap) of
    (Just _idxOid, Nothing) -> Just ("deleted", path)
    (Just idxOid, Just workOid) ->
      if idxOid /= workOid then Just ("modified", path) else Nothing
    _ -> Nothing

-- Compute "Untracked files"
-- Files in working directory but not in index or HEAD
computeUntrackedFiles :: Map FilePath String -> Map FilePath String -> Map FilePath String -> [FilePath]
computeUntrackedFiles headMap indexMap workingMap =
  let wdPaths = Map.keys workingMap
  in filter (\p -> not (Map.member p indexMap) && not (Map.member p headMap)) wdPaths

formatStatusOutput :: String -> [(String, FilePath)] -> [(String, FilePath)] -> [FilePath] -> String
formatStatusOutput branch changesToCommit changesNotStaged untracked =
  let branchLine = "On branch " ++ branch ++ "\n"
      commitSection = if null changesToCommit then "" else "Changes to be committed:\n" ++ formatChanges changesToCommit
      notStagedSection = if null changesNotStaged then "" else "Changes not staged for commit:\n" ++ formatChanges changesNotStaged
      untrackedSection = if null untracked then "" else "Untracked files:\n" ++ formatUntracked untracked
  in branchLine ++ "\n" ++ commitSection ++
     (if null commitSection then "" else "\n") ++
     notStagedSection ++
     (if null notStagedSection then "" else "\n") ++
     untrackedSection

formatChanges :: [(String, FilePath)] -> String
formatChanges xs =
  unlines [ "        " ++ statusType ++ ":   " ++ fp | (statusType, fp) <- xs ]

formatUntracked :: [FilePath] -> String
formatUntracked xs =
  unlines [ "        " ++ fp | fp <- xs ]