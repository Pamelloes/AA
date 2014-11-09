-- This is the bridge between HUnit tests and Cabal.
module Main where

import qualified Primitives_Test as P
import System.Exit (exitFailure)
import Test.HUnit

main = do
  counts <- runTestTT P.mainList
  if (errors counts/= 0) || (failures counts /= 0) then
    exitFailure
  else return ()
