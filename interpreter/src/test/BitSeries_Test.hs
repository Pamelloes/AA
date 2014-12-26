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
-- This module contains tests for the Opcodes module.
module BitSeries_Test where

import BitSeries
import Control.DeepSeq
import Test.HUnit

instance NFData Bit

-- Bit Tests
testBEq = TestLabel "Test bit equality" $
  TestList [ TestCase $ assertEqual "" T T
           , TestCase $ assertEqual "" F F
           , TestCase $ assertEqual "" Terminate Terminate
           , TestCase $ assertEqual "" Terminate F
           , TestCase $ assertBool "" (F/=T)
           , TestCase $ assertBool "" (Terminate/=T)
           ]
testBComp = TestLabel "Test bit comparisons" $
  TestList [ TestCase $ assertBool "" (F<T)
           , TestCase $ assertBool "" (Terminate<T)
           , TestCase $ assertBool "" (T>F)
           , TestCase $ assertBool "" (T>Terminate)
           ]

mainList = TestLabel "Bits" $
  TestList [ testBEq, testBComp ]
main = runTestTT mainList
