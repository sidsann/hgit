import AddTests (addTests)
import CommitTests (commitTests)
import InitTests (initTests)
import UtilTests (utilTests)
import LogTests(logTests)
import ParserTests (parserTests, prop_parseInput_correct)
import Test.HUnit ( runTestTT, Test(TestList) )
import Test.QuickCheck
    ( quickCheckWith, stdArgs, Testable, Args(maxSuccess) )
import BranchTests (branchTests)
import StatusTests (statusTests)
import SwitchTests (switchTests)

main :: IO ()
main = do
  -- Run HUnit Tests
  _ <-
    runTestTT $
      TestList
        [ parserTests,
          utilTests,
          initTests,
          addTests,
          commitTests,
          branchTests,
          logTests,
          switchTests,
          statusTests
        ]

  -- Run QuickCheck Properties
  quickCheckN 1000 prop_parseInput_correct

-- | Helper function to run QuickCheck with a specified number of tests
quickCheckN :: (Test.QuickCheck.Testable prop) => Int -> prop -> IO ()
quickCheckN n = quickCheckWith stdArgs {maxSuccess = n}