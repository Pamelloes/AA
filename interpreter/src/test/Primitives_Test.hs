-- This module contains tests for the Primitives module
module Primitives_Test where

import Control.Exception
import qualified Data.Map as M
import Opcodes
import qualified Opcodes_Test as P
import Primitives
import Test.HUnit
import TestException

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

-- Integer Tests
intError = ErrorCall "linteger: Reached program end"

testIEmptyP = TestLabel "Test Empty Program" $
  TestCase $ assertException intError (linteger [])
testIIncomplete1 = TestLabel "Test Incomplete Program 1" $
  TestCase $ assertException strError (linteger [False])
testIIncomplete2 = TestLabel "Test Incomplete Program 2" $
  TestCase $ assertException strError (linteger p)
  where p=[False]++(opcodes M.! "CS")
testIUnfinished = TestLabel "Test Unfinished Program" $
  TestCase $ assertException strError (linteger p)
  where p = [False]++(opcodes M.! "CS")++(replicate 4 False)

testIEmpty1 = TestLabel "Test Positive Empty Integer" $
  TestCase $ assertEqual "" ([],0) (linteger p)
  where p = [False]++(opcodes M.! "ES")
testIEmpty2 = TestLabel "Test Negative Empty Integer" $
  TestCase $ assertEqual "" ([],-1) (linteger p)
  where p = [True]++(opcodes M.! "ES")

testIL4 = TestLabel "Test Integer Length 4" $
  TestCase $ assertEqual "" ([],10) (linteger p)
  where p = [False]++(opcodes M.! "CS")++([True,False,True,False])
          ++(opcodes M.! "ES")
testIL8 = TestLabel "Test Integer Length 8" $
  TestCase $ assertEqual "" ([],49) (linteger p)
  where p = [False]++(opcodes M.! "CS")++([False,False,False,True])
          ++(opcodes M.! "CS")++([False,False,True,True])++(opcodes M.! "ES")
testINeg = TestLabel "Test Negative Integer" $
  TestCase $ assertEqual "" ([],-76) (linteger p)
  where p = [True]++(opcodes M.! "CS")++([False,True,False,False])
          ++(opcodes M.! "CS")++([True,False,True,True])++(opcodes M.! "ES")
testIExtra = TestLabel "Test Program Deletion" $
  TestCase $ assertEqual "" (p2,-3) (linteger (p1++p2))
  where p1 = [True]++(opcodes M.! "CS")++[True,True,False,True]
          ++(opcodes M.! "ES")
        p2 = replicate 100 True

intTests = TestLabel "Integer" $
  TestList [ testIEmptyP, testIIncomplete1, testIIncomplete2, testIUnfinished
           , testIEmpty1, testIEmpty2, testIL4, testIL8, testINeg, testIExtra ]

mainList = TestLabel "Primitives" $ TestList [ strTests, intTests ]
main = runTestTT $ TestList [ P.mainList, mainList]
