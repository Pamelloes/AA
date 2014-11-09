-- This module provides the assertException function for unit tests.
module TestException where

import Control.Exception
import Control.Monad 
import Test.HUnit

-- To use properly, "a" should be in the form "evaluate $ <statement>"
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
  handleJust isWanted (const $ return ()) $ do
    action
    assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)
