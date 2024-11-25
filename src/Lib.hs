{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Lib
    ( someDecl
    , Blob(..)
    , Index(..)
    , IndexEntry(..)
    , Commit(..)
    , OID(..)
    , Person(..)
    , FileMode(..)
    , Config(..)
    , Head(..)
    , Hashable(..)
    , computeOID
    , sha1Hash
    , serializeObject
    , deserializeObject
    , readIndex
    , writeIndex
    , updateIndex
    , readConfig
    , writeConfig
    , getAuthorInfo
    , readHead
    , updateHead
    , writeRef
    , readRef
    ) where

someDecl :: String
someDecl = "Hello CIS 5520"

-- Object Identifier (OID)
newtype OID = OID { unOID :: String }
  deriving (Eq, Ord, Show)

-- Git Objects
data Blob = Blob {
    blobContent :: String
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
    commitTimestamp  :: String     -- Using String to represent timestamp
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

-- Placeholder implementations for functions

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

readHead :: IO Head
readHead = undefined

updateHead :: Head -> IO ()
updateHead = undefined

writeRef :: String -> OID -> IO ()
writeRef = undefined

readRef :: String -> IO (Maybe OID)
readRef = undefined

class Hashable a where
    hashContent :: a -> String

    computeOID :: a -> OID
    computeOID x = OID (sha1Hash (hashContent x))

sha1Hash :: String -> String
sha1Hash = id -- Placeholder implementation

serializeObject :: (Hashable a) => a -> String
serializeObject = hashContent -- Placeholder implementation

deserializeObject :: String -> Either String (Either Blob (Either Tree Commit))
deserializeObject = undefined
