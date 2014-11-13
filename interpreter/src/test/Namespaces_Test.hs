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
-- This Module contains tests for the Namespaces module.
module Namespaces_Test where

import Control.Exception
import qualified Data.Map as M
import qualified LZipper_Test as L
import Namespaces
import Primitives
import Test.HUnit
import TestException

-- Namespace Tests
testDefaultNamespace = TestLabel "Test default namespace" $
  TestCase $ assertEqual "" (M.fromList [([],p)]) (defaultNamespace p)
  where p=[False,False,False,False]

testNmspValue = TestLabel "Test value retrieval" $
  TestCase $ assertEqual "" p (nmspValue [] $ defaultNamespace p)
  where p=replicate 5 True

testNmspDefValue = TestLabel "Test default value retrieval" $
  TestCase $ assertEqual "" [] (nmspValue [[False]] $ defaultNamespace p)
  where p=replicate 5 True
  
testNmspValueSet = TestLabel "Test value set" $
  TestCase $ assertEqual "" (M.fromList [([],p),(k,v)])
    (nmspValueSet k v $ defaultNamespace p)
  where p=replicate 10 False
        k=[[False,True],[True,False],[True,True,True,True]]
        v=[False,True,True,True,False]

testNmspValue2 = TestLabel "Test value retrieval 2" $
  TestCase $ assertEqual "" v
    (nmspValue k $ nmspValueSet k v $ defaultNamespace p)
  where p=replicate 5 True
        k=[replicate 4 True,[True,False,True,False],replicate 4 False]
        v=[False,False,False,True,False,False]

mainList = TestLabel "Namespaces" $
  TestList [ testDefaultNamespace, testNmspValue, testNmspDefValue 
           , testNmspValueSet, testNmspValue2
           ]
main = runTestTT $ TestList [ L.mainList, mainList ]
