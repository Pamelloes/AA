-- This module provides the assertException function for unit tests.
module TestException where

import Control.DeepSeq
import Control.Exception
import Control.Monad 
import Test.HUnit

-- To use properly, "a" should be in the form "evaluate $ <statement>"
assertException :: (Exception e, Eq e, NFData a) => e -> a -> IO ()
assertException ex action = 
  handle fail $ do
    handleJust isWanted (const $ return ()) $ do
      action `deepseq` return ()
      failure
  where isWanted = guard . (== ex)
        failure = assertFailure $ "Expected exception: " ++ show ex
        fail :: SomeException -> IO ()
        fail e = assertFailure $ "Got exception: " ++ show e
          ++ "\nExpected exception: " ++ show ex
