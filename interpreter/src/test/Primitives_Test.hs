-- This module contains tests for the Primitives module
module Primitives_Test where

import Primitives

import Opcodes
import qualified Opcodes_Test as P
import qualified Data.Map as M

import Test.HUnit
import TestException
import Control.Exception

-- String Tests
strError = ErrorCall "lstring: Reached program end"

testEmpty = TestLabel "Test Empty" $
  TestCase $ assertException strError (evaluate $ lstring [])
testIncomplete = TestLabel "Test Incomplete" $
  TestCase $ assertException strError
  (evaluate $ lstring ((opcodes M.! "CS") ++ (replicate 3 True)))
testUnfinished = TestLabel "Test Unfinished" $
  TestCase $ assertException strError
  (evaluate $ lstring ((opcodes M.! "CS") ++ (replicate 4 True)))

strTests = TestLabel "String" $
  TestList[ testEmpty, testIncomplete, testUnfinished]

mainList = TestLabel "Primitives" $ TestList [ strTests ]
main = runTestTT $ TestList [ P.mainList, mainList]
