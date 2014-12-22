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
-- This module contains tests for the Primitives module
module Primitives_Test where

import BitSeries
import qualified BitSeries_Test as B
import Control.DeepSeq
import Control.Exception
import qualified Data.Map as M
import Opcodes
import qualified Opcodes_Test as P
import DataType
import Test.HUnit
import TestException

instance NFData Primitive
instance Eq Primitive where
  (BString a)==(BString b)=a==b
  (BInteger a)==(BInteger b)=a==b
  (BRational a b)==(BRational c d)=(a==c) && (b==d)

-- String Tests
strError = ErrorCall "lstring: Reached program end"

testEmptyP = TestLabel "Test Empty Program"  $
  TestCase $ assertException strError (pstring [])
testIncomplete = TestLabel "Test Incomplete" $
  TestCase $ assertException strError (pstring p)
  where p=(opcodes M.! "CS") ++ (replicate 3 T)
testUnfinished = TestLabel "Test Unfinished" $
  TestCase $ assertException strError (pstring p)
  where p=(opcodes M.! "CS") ++ (replicate 4 T)

testEmpty = TestLabel "Test Empty String" $
  TestCase $ assertEqual "" ([],BString []) (pstring p)
  where p=opcodes M.! "ES"
testL4 = TestLabel "Test String Length 4" $
  TestCase $ assertEqual "" ([],BString str) (pstring p)
  where str=[F,T,T,F]
        p=(opcodes M.! "CS") ++ str ++ (opcodes M.! "ES")
testL8 = TestLabel "Test String Length 8" $
  TestCase $ assertEqual "" ([],BString $ s1++s2) (pstring p)
  where s1=replicate 4 F
        s2=[F,T,F,T]
        p=(opcodes M.! "CS")++s1++(opcodes M.! "CS")++s2++(opcodes M.! "ES")
testExtra = TestLabel "Test Program Deletion" $
  TestCase $ assertEqual "" (p2,BString str) $ pstring (p1++p2)
  where str=replicate 4 T
        p1=(opcodes M.! "CS")++str++(opcodes M.! "ES")
        p2=replicate 23 F

testLstring = TestLabel "Test lstring" $
  TestCase $ assertEqual "" (BString [T,T,T,T]) (lstring p)
  where p=(opcodes M.! "CS")++[T,T,T,T]++(opcodes M.! "ES")
testCs1 = TestLabel "Test cstring from BStatement" $
  TestCase $ assertEqual "" (p,BString s) (cstring (p,BStatement))
  where s=[T,T,T,T]
        p=(opcodes M.! "CS")++s++(opcodes M.! "ES")
testCs2 = TestLabel "Test cstring from BString" $
  TestCase $ assertEqual "" (p,BString s) (cstring (p,BString s))
  where s=[T,T,T,T]
        p=(opcodes M.! "CS")++[F,T,F,T]++(opcodes M.! "ES")

strTests = TestLabel "String" $
  TestList[ testEmptyP, testIncomplete, testUnfinished, testEmpty, testL4, testL8
          , testExtra, testLstring, testCs1, testCs2 ]

-- Integer Tests
intError = ErrorCall "linteger: Reached program end"

testIEmptyP = TestLabel "Test Empty Program" $
  TestCase $ assertException intError (pinteger [])
testIIncomplete1 = TestLabel "Test Incomplete Program 1" $
  TestCase $ assertException strError (pinteger [F])
testIIncomplete2 = TestLabel "Test Incomplete Program 2" $
  TestCase $ assertException strError (pinteger p)
  where p=[F]++(opcodes M.! "CS")
testIUnfinished = TestLabel "Test Unfinished Program" $
  TestCase $ assertException strError (pinteger p)
  where p = [F]++(opcodes M.! "CS")++(replicate 4 F)

testIEmpty1 = TestLabel "Test Positive Empty Integer" $
  TestCase $ assertEqual "" ([],BInteger 0) (pinteger p)
  where p = [F]++(opcodes M.! "ES")
testIEmpty2 = TestLabel "Test Negative Empty Integer" $
  TestCase $ assertEqual "" ([],BInteger (-1)) (pinteger p)
  where p = [T]++(opcodes M.! "ES")

testIL4 = TestLabel "Test Integer Length 4" $
  TestCase $ assertEqual "" ([],BInteger 10) (pinteger p)
  where p = [F]++(opcodes M.! "CS")++([T,F,T,F])
          ++(opcodes M.! "ES")
testIL8 = TestLabel "Test Integer Length 8" $
  TestCase $ assertEqual "" ([],BInteger 49) (pinteger p)
  where p = [F]++(opcodes M.! "CS")++([F,F,F,T])
          ++(opcodes M.! "CS")++([F,F,T,T])++(opcodes M.! "ES")
testINeg = TestLabel "Test Negative Integer" $
  TestCase $ assertEqual "" ([],BInteger (-76)) (pinteger p)
  where p = [T]++(opcodes M.! "CS")++([F,T,F,F])
          ++(opcodes M.! "CS")++([T,F,T,T])++(opcodes M.! "ES")
testIExtra = TestLabel "Test Program Deletion" $
  TestCase $ assertEqual "" (p2,BInteger (-3)) (pinteger (p1++p2))
  where p1 = [T]++(opcodes M.! "CS")++[T,T,F,T]
          ++(opcodes M.! "ES")
        p2 = replicate 100 T

testLinteger = TestLabel "Test linteger " $
  TestCase $ assertEqual "" (BInteger 15) (linteger p)
  where p=[F]++(opcodes M.! "CS")++[T,T,T,T]++(opcodes M.! "ES")
testCi1 = TestLabel "Test cinteger from BStatement" $
  TestCase $ assertEqual "" (p,BInteger (-4)) (cinteger (p,BStatement))
  where p=[T]++(opcodes M.! "CS")++[T,T,F,F]++(opcodes M.! "ES")
testCi2 = TestLabel "Test cinteger from BInteger" $
  TestCase $ assertEqual "" (p,BInteger (-9)) (cinteger (p,BInteger (-9)))
  where p=[F]++(opcodes M.! "CS")++[F,T,F,T]++(opcodes M.! "ES")

intTests = TestLabel "Integer" $
  TestList [ testIEmptyP, testIIncomplete1, testIIncomplete2, testIUnfinished
           , testIEmpty1, testIEmpty2, testIL4, testIL8, testINeg, testIExtra
           , testLinteger, testCi1, testCi2 ]

mainList = TestLabel "Primitives" $ TestList [ strTests, intTests ]
main = runTestTT $ TestList [ B.mainList, P.mainList, mainList]
