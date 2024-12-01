import AddTests (addTests)
import CommitTests (commitTests)
import InitTests (initTests)
import HashTests (hashTests)
import ParserTests (parserTests, prop_parseInput_correct)
import Test.HUnit
import Test.QuickCheck
    ( quickCheckWith, stdArgs, Testable, Args(maxSuccess) )

main :: IO ()
main = do
  -- Run HUnit Tests
  _ <-
    runTestTT $
      TestList
        [ parserTests,
          hashTests,
          initTests,
          addTests,
          commitTests
        ]

  -- Run QuickCheck Properties
  quickCheckN 1000 prop_parseInput_correct

-- | Helper function to run QuickCheck with a specified number of tests
quickCheckN :: (Test.QuickCheck.Testable prop) => Int -> prop -> IO ()
quickCheckN n = quickCheckWith stdArgs {maxSuccess = n}