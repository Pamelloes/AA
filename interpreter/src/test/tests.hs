-- This is the bridge between HUnit tests and Cabal.
import qualified Primitives_Test as P
import Test.HUnit

import System.Exit (exitFailure)

main = do
  counts <- runTestTT P.mainList
  if (errors counts/= 0) || (failures counts /= 0) then
    exitFailure
  else return ()
