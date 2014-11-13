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
-- This module contains tests for the LZipper module.
module LZipper_Test where

import Control.Monad
import qualified Data.Map as M
import LZipper
import Test.HUnit

-- LZipper Tests
testZipper = TestLabel "Test zipper creation" $
  TestCase $ assertEqual "" (l,[]) (zipper l)
  where l=[1,2,3,4,5]

testForward = TestLabel "Test forward movement" $
  TestCase $ assertEqual "" (Just (tail l,[head l])) (forward $ zipper l)
  where l="This is a test string"

testForwardFail = TestLabel "Test forward movement failure" $
  TestCase $ assertEqual "" Nothing (return (zipper l)>>=forward>>=forward)
  where l=[False]

testBackward = TestLabel "Test backward movement" $
  TestCase $ assertEqual "" (Just (tail l,[head l]))
    (return (zipper l)>>=forward>>=forward>>=backward)
  where l=[(1,2),(2,3),(3,4),(4,5),(5,6)]

testBackwardFail = TestLabel "Test backward movement failure" $
  TestCase $ assertEqual "" Nothing (backward $ zipper l)
  where l=["this","is","a","test","string"]

testUnzipper = TestLabel "Test unzipper" $
  TestCase $ assertEqual "" (Just l)
    (fmap unzipper $ return (zipper l)>>=forward>>=forward>>=backward)
  where l=[1,1,2,3,5,8,13]
  
testEnd = TestLabel "Test jumping to end of zipper" $
  TestCase $ assertEqual "" ([],reverse l) (end $ zipper l)
  where l=[1.0,1.1,1.2,1.3,1.4,1.5]

testStart = TestLabel "Test jumping to start of zipper" $
  TestCase $ assertEqual "" (Just $ zipper l) 
    (fmap start $ return (zipper l)>>=forward>>=forward)
  where l=[10000000000000000,2000000000000000000,523215351325326246246246]
  
testInfinite = TestLabel "Test with infinite lists" $
  TestCase $ assertEqual "" (Just l)
    (fmap unzipper $ return (zipper l)>>=forward>>=forward>>=forward
    >>=forward>>=forward>>=backward>>=backward>>=backward>>=forward)
  where l=[1,2..]
  
mainList = TestLabel "LZipper" $
  TestList [ testZipper, testForward, testForwardFail, testBackward
           , testBackwardFail, testUnzipper, testEnd, testStart
           , testInfinite]
main = runTestTT mainList
