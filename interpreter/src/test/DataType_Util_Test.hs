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
-- This module contains tests for the DataType.Util module
module DataType_Util_Test where

import BitSeries
import qualified BitSeries_Test as B
import Control.DeepSeq
import Control.Exception
import qualified Data.Map as M
import Opcodes
import qualified Opcodes_Test as P
import DataType
import qualified DataType_Test as D
import DataType.Util
import Test.HUnit
import TestException

-- String Tests
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

testBtS = TestLabel "Test inserting BString opcodes into BitSeries" $
  TestCase $ assertEqual "" p (bsToString s)
  where s=[T,T,F,F]++[T,T,F,F]
        p=(o "CS")++[T,T,F,F]++(o "CS")++[T,T,F,F]++(o "ES")
        o t = opcodes M.! t

strTests = TestLabel "String" $
  TestList[ testLstring, testCs1, testCs2, testBtS ]

-- Integer Tests
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
  TestList [ testLinteger, testCi1, testCi2 ]

-- Rational Tests
testLrational = TestLabel "Test lrational" $
  TestCase $ assertEqual "" (BRational 15 (-3)) (lrational p)
  where p= [F]++(opcodes M.! "CS")++[T,T,T,T]++(opcodes M.! "ES")
         ++[T]++(opcodes M.! "CS")++[T,T,F,T]++(opcodes M.! "ES")
testCr1 = TestLabel "Test crational from BStatement" $
  TestCase $ assertEqual "" (p,BRational (-4) 1) (crational (p,BStatement))
  where p= [T]++(opcodes M.! "CS")++[T,T,F,F]++(opcodes M.! "ES")
         ++[F]++(opcodes M.! "CS")++[F,F,F,T]++(opcodes M.! "ES")
testCr2 = TestLabel "Test crational from BRational" $
  TestCase $ assertEqual "" (p,BRational 7 1234) (crational (p,BRational 7 1234))
  where p= [F]++(opcodes M.! "CS")++[T,T,T,T]++(opcodes M.! "ES")
         ++[F]++(opcodes M.! "ES")

rationalTests = TestLabel "Rational" $
  TestList [ testLrational, testCr1, testCr2 ]

-- Namespace Tests
maid :: ANmsp -> DataType
maid x = ([],BNmspId $ Left x)

mrid :: RNmsp -> DataType
mrid x = ([],BNmspId $ Right x)

testLNmsp = TestLabel "Test lnmsp" $
  TestCase $ assertEqual "" (snd $ maid id) (lnmsp p)
  where id=[[T,T,T,T]]
        p=(o "AN")++(o "CN")++(o "CS")++[T,T,T,T]++(o "ES")++(o "EN")
        o s=opcodes M.! s

testCn1 = TestLabel "Test cnmsp 1" $
  TestCase $ assertEqual "" (maid id) (cnmsp $ maid id)
  where id=[[T,T,F,F]]

testCn2 = TestLabel "Test cnmsp 2" $
  TestCase $ assertEqual "" (p,snd $ maid id) (cnmsp (p,BStatement))
  where id=[[F,T,F,F],[T,F,F,F]]
        p=(o "AN")++(o "CN")++(o "CS")++id!!0++(o "ES")++(o "CN")
         ++(o "CS")++id!!1++(o "ES")++(o "EN")
        o s=opcodes M.! s

nmspTests = TestLabel "Namespace" $
  TestList [ testLNmsp, testCn1, testCn2 ]

mainList = TestLabel "DataType.Util" $ 
  TestList [ strTests, intTests, rationalTests, nmspTests ]
main = runTestTT $ TestList [ B.mainList, P.mainList, D.mainList, mainList]
