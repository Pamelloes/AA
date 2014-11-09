-- This module contains tests for the Opcodes module.
module Opcodes_Test where

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
  "Should recognize opcode of length 4." ([],True) (hasOpcode (opcodes M.! "TRA")
  "TRA")

testPos = TestList [testL1, testL2, testL4, testL5, testL7]

testN1 = TestCase $ assertEqual
  "Should not recognize opcode of length 1." (pr,False) (hasOpcode pr "ET")
  where pr=[False,False]

testN2 = TestCase $ assertEqual
  "Should not recognize opcode of length 1." (pr,False) (hasOpcode pr "ERN")
  where pr=[False,True,False,True]

testN4 = TestCase $ assertEqual
  "Should not recognize opcode of length 4." (pr,False) (hasOpcode pr "BN")
  where pr=[False,False,False,True,True,True]

testN5 = TestCase $ assertEqual
  "Should not recognize opcode of length 5." (pr,False) (hasOpcode pr "BO")
  where pr=[False,False,True,True,False,False]

testN7 = TestCase $ assertEqual
  "Should not recognize opcode of length 7." (pr,False) (hasOpcode pr "TTL")
  where pr=[True,False,True,True,True,True,True,False]

testNeg = TestList [testN1, testN2, testN4, testN5, testN7]

mainList = TestList [testEmpty, testPos, testNeg] 
main = runTestTT mainList
