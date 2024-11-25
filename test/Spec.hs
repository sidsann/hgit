import Lib
import Test.HUnit
import Test.QuickCheck

-- >>> someDecl

main :: IO ()
main = do
  putStrLn someDecl
  putStrLn "Test suite not yet implemented"

prop_computeOIDConsistency :: Blob -> Bool
prop_computeOIDConsistency blob =
  computeOID blob == OID (sha1Hash (hashContent blob))

testRefs :: Test
testRefs =
  TestList
    [ "Write and read ref"
        ~: let refName = "refs/heads/test-branch"
               oid = OID "testoid"
            in do
                 writeRef refName oid
                 result <- readRef refName
                 result ~?= Just oid
    ]

testHead :: Test
testHead =
  TestList
    [ "Update and read HEAD"
        ~: let headRef = SymbolicRef "refs/heads/main"
            in do
                 updateHead headRef
                 head' <- readHead
                 head' ~?= headRef
    ]

testGetAuthorInfo :: Test
testGetAuthorInfo = TestCase $ do
  let config = Config (Just "Bob") (Just "bob@example.com")
  writeConfig config
  author <- getAuthorInfo
  author @?= Person "Bob" "bob@example.com"

prop_indexRoundTrip :: Index -> Property
prop_indexRoundTrip index = ioProperty $ do
  writeIndex index
  index' <- readIndex
  return (index' == index)

testIndex :: Test
testIndex =
  TestList
    [ "Update and read index"
        ~: let entry = IndexEntry "test.txt" (OID "bloboid") ModeBlob
               index = Index [entry]
            in do
                 writeIndex index
                 index' <- readIndex
                 index' ~?= index
    ]

test_serializeDeserializeCommit :: Test
test_serializeDeserializeCommit =
  "serialize and deserialize Commit"
    ~: let commit =
             Commit
               { commitTreeOID = OID (BS.pack "treeoid123"),
                 commitParentOIDs = [OID (BS.pack "parentoid456")],
                 commitAuthor = Person "Alice" "alice@example.com",
                 commitMessage = "Initial commit",
                 commitTimestamp = parseTimeOrError True defaultTimeLocale "%F %T %Z" "2023-10-01 12:34:56 UTC"
               }
           serialized = serializeObject commit
           deserialized = deserializeObject serialized
        in deserialized ~?= Right (Right commit)

-- as you can see, we will generally test the functions which compose the actual
-- commands and by ensuring that these auxiliary functions are all correct, we can
-- guarantee the correctness of the overall commands as well. Still need to come up with
-- an appropriate model to actually test the full commands themselves but more ideas for
-- validity tests:

-- Test that initializing a repository creates the necessary directories and files.
-- Test behavior when initializing in a directory that already has a repository.
-- Test parsing of valid commands with various arguments and options.
-- Test that writeTree constructs a tree object representing the current index.
-- Test writing trees for:
--     A flat directory structure.
--     A nested directory structure.
-- Test that traverseTree correctly lists all entries in a tree.
-- Test traversal of trees with nested subtrees.
-- Test that createCommit creates a commit with the correct tree OID, parent OIDs, author, message, and timestamp.
-- Test committing when there are staged changes in the index.
-- Test committing with an empty index (should prevent or warn).
-- Test that createBranch creates a new branch with the specified name and OID.
-- Test creating a branch that already exists (should handle or prevent duplicates).
-- Test creating a branch with invalid names (e.g., names with illegal characters).
