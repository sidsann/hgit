-- * Entry point for a Haskell application
-- Typically, this file is short and most code is part of a reusable library
-- module Main where


-- -- >>> someDecl


{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main where

import System.FilePath (FilePath)
import Lib
import Data.Array.Byte (ByteArray)
import System.Directory.Internal.Prelude (EpochTime)

main :: IO ()
main = putStrLn someDecl

-- Object Identifier (OID)
newtype OID = OID { unOID :: ByteArray }
  deriving (Eq, Ord, Show)

-- Git Objects
data Blob = Blob {
    blobContent :: ByteArray
  } deriving (Eq, Show)

data FileMode =
    ModeBlob        
  | ModeExecutable 
  | ModeTree           
  deriving (Eq, Show)

data TreeEntry = TreeEntry {
    entryMode :: FileMode,   
    entryOID  :: OID,        
    entryName :: FilePath    
  } deriving (Eq, Show)

data Tree = Tree {
    treeEntries :: [TreeEntry]
  } deriving (Eq, Show)

data Person = Person {
    personName  :: String,  
    personEmail :: String    
  } deriving (Eq, Show)

data Commit = Commit {
    commitTreeOID    :: OID,        
    commitParentOIDs :: [OID],     
    commitAuthor     :: Person,     
    commitMessage    :: String,     
    commitTimestamp  :: EpochTime     
  } deriving (Eq, Show)

-- Index (Staging Area)
data IndexEntry = IndexEntry {
    indexEntryPath :: FilePath,     
    indexEntryOID  :: OID,          
    indexEntryMode :: FileMode      
  } deriving (Eq, Show)

data Index = Index {
    indexEntries :: [IndexEntry]
  } deriving (Eq, Show)

-- References (Refs)
data Ref = Ref {
    refName :: String,              
    refOID  :: OID                  
  } deriving (Eq, Show)

-- HEAD Pointer
data Head =
    SymbolicRef String              
  | DetachedHead OID                
  deriving (Eq, Show)

-- Configuration
data Config = Config {
    configUserName  :: Maybe String,
    configUserEmail :: Maybe String
  } deriving (Eq, Show)

readIndex :: IO Index
readIndex = undefined

writeIndex :: Index -> IO ()
writeIndex = undefined

updateIndex :: [IndexEntry] -> IO ()
updateIndex = undefined

readConfig :: IO Config
readConfig = undefined

writeConfig :: Config -> IO ()
writeConfig = undefined

getAuthorInfo :: IO Person
getAuthorInfo = undefined

writeTree :: Index -> IO OID
writeTree = undefined

traverseTree :: Tree -> IO [TreeEntry]
traverseTree = undefined

createCommit :: String -> IO OID
createCommit = undefined

amendCommit :: String -> IO OID
amendCommit = undefined

getCommit :: OID -> IO Commit
getCommit = undefined

listBranches :: IO [String]
listBranches = undefined

createBranch :: String -> OID -> IO ()
createBranch = undefined

deleteBranch :: String -> IO ()
deleteBranch = undefined

renameBranch :: String -> String -> IO ()
renameBranch = undefined

readHead :: IO Head
readHead = undefined

updateHead :: Head -> IO ()
updateHead = undefined

getCurrentCommitOID :: IO (Maybe OID)
getCurrentCommitOID = undefined

class Hashable a where
    hashContent :: a -> ByteArray

    computeOID :: a -> OID
    computeOID = OID . sha1Hash . hashContent

sha1Hash :: ByteArray -> ByteArray
sha1Hash = undefined 

serializeObject :: (Hashable a) => a -> OID
serializeObject = undefined

deserializeObject :: OID -> Either String (Either Blob (Either Tree Commit))
deserializeObject = undefined

writeObject :: (Hashable a) => a -> IO OID
writeObject = undefined

readObject :: OID -> IO ByteArray
readObject = undefined