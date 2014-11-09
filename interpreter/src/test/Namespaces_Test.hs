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
import Namespaces
import Primitives
import Test.HUnit
import TestException

-- Namespace Tests

verifyGN = TestLabel "Verify Global Namespace" $
  TestCase $ assertEqual "" gn (globalNamespace p)
  where gn = (Namespace [] p (M.fromList []) Nothing)
        p = replicate 72 False

bNmsp :: BString -> Namespace -> Namespace
bNmsp n p = Namespace n [] (M.fromList []) (Just p)
verifyBNmsp = TestLabel "Verify Basic Namespace Generator" $
  TestCase $ assertEqual "" nm (bNmsp [True] (globalNamespace []))
  where nm = Namespace [True] [] (M.fromList []) (Just $ globalNamespace [])

cNmsp :: BString -> Namespace -> [Namespace] -> Namespace
cNmsp n p c = Namespace n [] children (Just p)
  where chilren = M.fromList [(name x,x) | x <- c]
verifyCNmsp = TestLabel "Verify Complex Namespace Generator" $
  TestCase $ assertEqual "" nm (cNmsp [False] (globalNamespace []) children)
  where c1 = bNmsp 

mainList = TestLabel "Namespaces" $ TestList [verifyGN, verifyBNmsp]
main = runTestTT mainList
