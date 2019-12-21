import qualified D1                             ( tests )
import qualified D2                             ( tests )

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "All tests" [D1.tests, D2.tests]




