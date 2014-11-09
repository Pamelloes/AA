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

testEmptyP = TestLabel "Test Empty Program"  $
  TestCase $ assertException strError (lstring [])
testIncomplete = TestLabel "Test Incomplete" $
  TestCase $ assertException strError (lstring p)
  where p=(opcodes M.! "CS") ++ (replicate 3 True)
testUnfinished = TestLabel "Test Unfinished" $
  TestCase $ assertException strError (lstring p)
  where p=(opcodes M.! "CS") ++ (replicate 4 True)

testEmpty = TestLabel "Test Empty String" $
  TestCase $ assertEqual "" ([],[]) (lstring p)
  where p=opcodes M.! "ES"
testL4 = TestLabel "Test String Length 4" $
  TestCase $ assertEqual "" ([],str) (lstring p)
  where str=[False,True,True,False]
        p=(opcodes M.! "CS") ++ str ++ (opcodes M.! "ES")
testL8 = TestLabel "Test String Length 8" $
  TestCase $ assertEqual "" ([],s1++s2) (lstring p)
  where s1=replicate 4 False
        s2=[False,True,False,True]
        p=(opcodes M.! "CS")++s1++(opcodes M.! "CS")++s2++(opcodes M.! "ES")
testExtra = TestLabel "Test Program Deletion" $
  TestCase $ assertEqual "" (p2,str) $ lstring (p1++p2)
  where str=replicate 4 True
        p1=(opcodes M.! "CS")++str++(opcodes M.! "ES")
        p2=replicate 23 False

strTests = TestLabel "String" $
  TestList[ testEmptyP, testIncomplete, testUnfinished, testEmpty, testL4, testL8
          , testExtra]

mainList = TestLabel "Primitives" $ TestList [ strTests ]
main = runTestTT $ TestList [ P.mainList, mainList]
