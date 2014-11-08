module TestException where

import Control.Exception
import Control.Monad 
import Test.HUnit

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
  handleJust isWanted (const $ return ()) $ do
    action
    assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)
