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
module Opcodes_Test where

import BitSeries
import qualified BitSeries_Test as B
import Control.DeepSeq
import Control.Exception
import qualified Data.Map as M
import Opcodes
import Test.HUnit
import TestException

-- Opcode Tests
opError = ErrorCall "hasOpcode: Reached program end"

testEmpty = TestCase $ assertException opError (hasOpcode [] "ES")

testL1 = TestCase $ assertEqual
  "Should recognize opcode of length 1." ([],True) (hasOpcode (opcodes M.! "ES")
  "ES")

testL2 = TestCase $ assertEqual
  "Should recognize opcode of length 2." ([],True) (hasOpcode (opcodes M.! "LI")
  "LI")

testL4 = TestCase $ assertEqual
  "Should recognize opcode of length 4." ([],True) (hasOpcode (opcodes M.! "OU")
  "OU")

testL5 = TestCase $ assertEqual
  "Should recognize opcode of length 5." ([],True) (hasOpcode (opcodes M.! "BX")
  "BX")

testL7 = TestCase $ assertEqual
  "Should recognize opcode of length 7." ([],True) (hasOpcode (opcodes M.! "TS")
  "TS")

testPos = TestList [testL1, testL2, testL4, testL5, testL7]

testN1 = TestCase $ assertEqual
  "Should not recognize opcode of length 1." (pr,False) (hasOpcode pr "EN")
  where pr=[T,F]

testN2 = TestCase $ assertEqual
  "Should not recognize opcode of length 2." (pr,False) (hasOpcode pr "ERN")
  where pr=[F,T,F,T]

testN4 = TestCase $ assertEqual
  "Should not recognize opcode of length 4." (pr,False) (hasOpcode pr "BN")
  where pr=[F,F,F,T,T,T]

testN5 = TestCase $ assertEqual
  "Should not recognize opcode of length 5." (pr,False) (hasOpcode pr "BO")
  where pr=[F,F,T,T,F,F]

testN7 = TestCase $ assertEqual
  "Should not recognize opcode of length 7." (pr,False) (hasOpcode pr "TX")
  where pr=[T,F,T,T,T,T,T,F]

testNeg = TestList [testN1, testN2, testN4, testN5, testN7]

mainList = TestList [testEmpty, testPos, testNeg] 
main = runTestTT $ TestList [B.mainList, mainList]
