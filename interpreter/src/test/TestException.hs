{-
Advanced Assembly Interpreter
Copyright (c) 2014 Joshua Brot

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
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
