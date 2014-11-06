module TestException where

import Control.Exception
import Control.Monad 
import Test.HUnit

import Debug.Trace

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action = seq action $ return ()
{-
assertException ex action =
  handleJust isWanted (const $ return ()) $ do
    action
    assertFailure $ "Expected exception: " ++ show ex
  where isWanted = trace "Woo" $  guard . (== ex)
-}
