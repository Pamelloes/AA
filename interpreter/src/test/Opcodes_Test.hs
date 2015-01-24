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

import qualified BitSeries_Test as B
import Language.AA.BitSeries
import Language.AA.Opcodes
import Test.HUnit
import TestUtil

-- Opcode Tests
testEmpty = ptestf "Fail empty string." (mopc "ES") []

testL1 = ptest "Should recognize opcode of length 1." (o "ES") (mopc "ES") 
  (o "ES")

testL2 = ptest "Should recognize opcode of length 2." (o "LI") (mopc "LI")
  (o "LI")

testL4 = ptest "Should recognize opcode of length 4." (o "OU") (mopc "OU")
  (o "OU")

testL5 = ptest "Should recognize opcode of length 5." (o "BX") (mopc "BX")
  (o "BX")

testL7 = ptest "Should recognize opcode of length 7." (o "TS") (mopc "TS")
  (o "TS")

testPos = TestList [testL1, testL2, testL4, testL5, testL7]

testN1 = ptestf "Should not recognize opcode of length 1." (mopc "EN") [T,F]

testN2 = ptestf "Should not recognize opcode of length 2." (mopc "ERN")
  [F,T,F,T]

testN4 = ptestf "Should not recognize opcode of length 4." (mopc "BN")
  [F,F,F,T,T,T]

testN5 = ptestf "Should not recognize opcode of length 5." (mopc "BO")
  [F,F,T,T,F,F]

testN7 = ptestf "Should not recognize opcode of length 7." (mopc "TX")
  [T,F,T,T,T,T,T,F]

testNeg = TestList [testN1, testN2, testN4, testN5, testN7]

mainList = TestList [testEmpty, testPos, testNeg] 
main = runTestTT $ TestList [B.mainList, mainList]
