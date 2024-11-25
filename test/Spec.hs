import Lib
import Test.HUnit
import Test.QuickCheck
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (void)

instance Hashable Blob where
    hashContent (Blob content) = content

instance Arbitrary Blob where
    arbitrary = Blob <$> arbitrary

instance Arbitrary OID where
    arbitrary = OID <$> arbitrary

instance Arbitrary Person where
    arbitrary = Person <$> arbitrary <*> arbitrary

instance Arbitrary FileMode where
    arbitrary = elements [ModeBlob, ModeExecutable, ModeTree]

instance Arbitrary IndexEntry where
    arbitrary = IndexEntry <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Index where
    arbitrary = Index <$> arbitrary

instance Arbitrary Head where
    arbitrary = oneof [SymbolicRef <$> arbitrary, DetachedHead <$> arbitrary]


test_computeOIDConsistency :: Test
test_computeOIDConsistency =
  "computeOID consistency for Blob" ~:
    let blob = Blob "Hello, world!"
        oid1 = computeOID blob
        oid2 = OID (sha1Hash (hashContent blob))
    in oid1 ~?= oid2

test_serializeObject :: Test
test_serializeObject =
    "serializeObject for Blob" ~:
    let blob = Blob "Test content"
        serialized = serializeObject blob
    in serialized ~?= "Test content"

test_ConfigReadWrite :: Test
test_ConfigReadWrite = TestCase $ do
    let config = Config (Just "Alice") (Just "alice@example.com")
    Lib.writeConfig config
    config' <- Lib.readConfig
    assertEqual "Config read after write" config config'

test_getAuthorInfo :: Test
test_getAuthorInfo = TestCase $ do
    let config = Config (Just "Bob") (Just "bob@example.com")
    writeConfig config
    author <- getAuthorInfo
    assertEqual "getAuthorInfo returns correct Person" (Person "Bob" "bob@example.com") author

test_HeadReadWrite :: Test
test_HeadReadWrite = TestCase $ do
    let headRefValue = SymbolicRef "refs/heads/feature"
    updateHead headRefValue
    head' <- readHead
    assertEqual "Head read after update" headRefValue head'

test_IndexReadWrite :: Test
test_IndexReadWrite = TestCase $ do
    let index = Index [IndexEntry "file.txt" (OID "bloboid") ModeBlob]
    writeIndex index
    index' <- readIndex
    assertEqual "Index read after write" index index'

prop_computeOIDConsistency :: Blob -> Bool
prop_computeOIDConsistency blob =
  computeOID blob == OID (sha1Hash (hashContent blob))

prop_PersonEquality :: Person -> Bool
prop_PersonEquality person = person == person

prop_IndexEquality :: Index -> Bool
prop_IndexEquality index = index == index

main :: IO ()
main = do
    -- Run HUnit tests
    _ <- runTestTT $ TestList [
        test_computeOIDConsistency,
        test_serializeObject,
        test_ConfigReadWrite,
        test_getAuthorInfo,
        test_HeadReadWrite,
        test_IndexReadWrite
      ]
    quickCheck prop_computeOIDConsistency
    quickCheck prop_PersonEquality
    quickCheck prop_IndexEquality
    putStrLn "All tests completed."
