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
-- This module contains tests for the DataType module's String, Integer, and
-- Rational sections.
module DataTypePrime_Test where

import BitSeries
import qualified BitSeries_Test as B
import Control.DeepSeq
import Control.Exception
import qualified Data.Map as M
import Opcodes
import qualified Opcodes_Test as P
import DataTypePrime
import Test.HUnit
import TestException
import TestUtil

instance NFData RNmspS
instance Eq RNmspS where
  (Child a)==(Child b)=a==b
  Parent==Parent=True
  _==_=False

instance NFData Primitive
instance Eq Primitive where
  (BString a)==(BString b)=a==b
  (BInteger a)==(BInteger b)=a==b
  (BRational a b)==(BRational c d)=(a==c) && (b==d)
  (BNmspId a)==(BNmspId b)=a==b
  (BStatement)==(BStatement)=True

-- DataType Terminate Truncator
tD :: DataType -> DataType
tD (b,p) = (B.tT b,p)

-- String Tests
strError = ErrorCall "lstring: Reached program end"

testEmptyP = ptestf "Test Empty Program" pstring []
testIncomplete = ptestf "Test Incomplete" pstring $ (o "CS")++[T,T,T]
testUnfinished = ptestf "Test Unfinished" pstring $ (o "CS")++[T,T,T,T]

testEmpty = ptest "Test Empty String" (o "ES",BString []) pstring (o "ES")
testL4 = ptest "Test String Length 4" (p,BString [F,T,T,F]) pstring p
  where p=(o "CS")++[F,T,T,F]++(o "ES")
testL8 = ptest "Test String Length 8" (p,BString s) pstring p
  where s=[F,F,F,F, F,T,F,T]
        p=(o "CS")++[F,F,F,F]++(o "CS")++[F,T,F,T]++(o "ES")
testExtra = ptest "Test Program Deletion" (p2,BString [T,T,T,T])
  (pstring>>pstring) (p1++p2)
  where p1=(o "CS")++[F,F,F,F]++(o "ES")
        p2=(o "CS")++[T,T,T,T]++(o "ES")

strTests = TestLabel "String" $
  TestList[ testEmptyP, testIncomplete, testUnfinished, testEmpty, testL4, testL8
          , testExtra ]

-- Integer Tests
intError = ErrorCall "linteger: Reached program end"

testIEmptyP = ptestf "Test Empty Program" pinteger []
testIIncomplete1 = ptestf "Test Incomplete Program 1" pinteger [F]
testIIncomplete2 = ptestf "Test Incomplete Program 2" pinteger ([F]++(o "CS"))
testIUnfinished = ptestf "Test Unfinished Program" pinteger
  ([F]++(o "CS")++[F,F,F,F])

testIEmpty1 = ptest "Test Positive Empty Integer" (p,BInteger 0) pinteger p
  where p=[F]++(o "ES")
testIEmpty2 = ptest "Test Negative Empty Integer" (p,BInteger (-1)) pinteger p
  where p=[T]++(o "ES")

testIL4 = ptest "Test Integer Length 4" (p,BInteger 10) pinteger p
  where p=[F]++(o "CS")++[T,F,T,F]++(o "ES")
testIL8 = ptest "Test Integer Length 8" (p,BInteger 49) pinteger p
  where p=[F]++(o "CS")++[F,F,F,T] ++(o "CS")++[F,F,T,T]++(o "ES")
testINeg = ptest "Test Negative Integer" (p,BInteger (-76)) pinteger p
  where p=[T]++(o "CS")++[F,T,F,F]++(o "CS")++[T,F,T,T]++(o "ES")
testIExtra = ptest "Test Program Deletion" (p2,BInteger (-3))
  (pinteger>>pinteger) (p1++p2)
  where p1=[F]++(o "CS")++[F,T,F,F]++(o "ES")
        p2=[T]++(o "CS")++[T,T,F,T]++(o "ES")

intTests = TestLabel "Integer" $
  TestList [ testIEmptyP, testIIncomplete1, testIIncomplete2, testIUnfinished
           , testIEmpty1, testIEmpty2, testIL4, testIL8, testINeg, testIExtra ]

-- Rational Tests
rationalError = ErrorCall "lrational: Reached program end"

testREmptyP = ptestf "Test Empty Program" prational []
testRIncomplete1 = ptestf "Test Incomplete Program 1" prational [F]
testRIncomplete2 = ptestf "Test Incomplete Program 2" prational ([F]++(o "CS"))
testRUnfinished = ptestf "Test Unfinished Program" prational
  ([F]++(o "CS")++[F,F,F,F]++(o "ES"))

testREDiv0 = ptest "Test Empty Division By 0" (p,BRational 0 0) prational p
  where p=[F]++(o "ES")++[F]++(o "ES")
testRDiv0 = ptest "Test Division By 0" (p,BRational 0 0) prational p
  where p=[F]++(o "CS")++[T,T,T,T]++(o "ES")++[F]++(o "ES")
testREmpty1 = ptest "Test Empty 0" (p,BRational 0 (-1)) prational p
  where p=[F]++(o "ES")++[T]++(o "ES")
testREmpty2 = ptest "Test Empty 1" (p,BRational (-1) (-1)) prational p
  where p=[T]++(o "ES")++[T]++(o "ES")

testRL44 = ptest "Test Rational Length 4/4" (p,BRational 6 1) prational p
  where p=[F]++(o "CS")++[F,T,T,F]++(o "ES")++[F]++(o "CS")++[F,F,F,T]++(o "ES")
testRL84 = ptest "Test Rational Length 8/4" (p,BRational 50 3) prational p
  where p= [F]++(o "CS")++[F,F,T,F]++(o "CS")++[F,F,T,T]++(o "ES")++[F]
         ++(o "CS")++[F,F,T,T]++(o "ES")
testRL48 = ptest "Test Rational Length 4/8" (p,BRational 7 106) prational p
  where p= [F]++(o "CS")++[F,T,T,T]++(o "ES")++[F]++(o "CS")++[T,F,T,F]
         ++(o "CS")++[F,T,T,F]++(o "ES")
testRL88 = ptest "Test Rational Length 8/8" (p,BRational 249 16) prational p
  where p= [F]++(o "CS")++[T,F,F,T]++(o "CS")++[T,T,T,T]++(o "ES")++[F]
         ++(o "CS")++[F,F,F,F]++(o "CS")++[F,F,F,T]++(o "ES")
testRExtra = ptest "Test Program Deletion" (p2,BRational (-3) (1))
  (prational>>prational) (p1++p2)
  where p1= [F]++(o "CS")++[T,F,F,F]++(o "ES")++[F]++(o "CS")++[F,F,F,T]
          ++(o "ES")
        p2= [T]++(o "CS")++[T,T,F,T]++(o "ES")++[F]++(o "CS")++[F,F,F,T]
          ++(o "ES")

rationalTests = TestLabel "Rational" $
  TestList [ testREmptyP, testRIncomplete1, testRIncomplete2, testRUnfinished
           , testREDiv0, testRDiv0, testREmpty1, testREmpty2, testRL44, testRL84
           , testRL48, testRL88, testRExtra ]

-- Namespace Tests
maid :: ANmsp -> DataType
maid x = ([],BNmspId $ Left x)

mrid :: RNmsp -> DataType
mrid x = ([],BNmspId $ Right x)

testPNmspA = ptest "Test loading absolute namespace" (p,BNmspId$Left id) pnmsp p
  where id=[[T,F,T,F],[F,F,F,F],[T,T,T,T]]
        p=(o "AN")++(o "CN")++(o "CS")++id!!0++(o "ES")++(o "CN")
          ++(o "CS")++id!!1++(o "ES")++(o "CN")++(o "CS")++id!!2
          ++(o "ES")++(o "EN")

testPNmspR = ptest "Test loading relative namespace" (p,BNmspId$Right id)
  pnmsp p
  where id=[Parent,Child [F,F,F,F], Child [T,T,T,T]]
        p=(o "RN")++(o "PN")++(o "CN")++(o "CS")++[F,F,F,F]++(o "ES")
          ++(o "CN")++(o "CS")++[T,T,T,T]++(o "ES")++(o "ERN")

testPNmspExtra = ptest "Test loading namespace with extra program"
  (p2,BNmspId$Right [Parent]) (pnmsp>>pnmsp) (p1++p2)
    where p1=(o "AN")++(o "EN")
          p2=(o "RN")++(o "PN")++(o "ERN")

testPNmspF = ptestf "Test loading short namespace" pnmsp []

testPNmspF2 = ptestf "Test loading short namespace 2" pnmsp p
  where p=(o "AN")++(o "CN")++(o "CS")

nmspTests = TestLabel "Namespace" $
  TestList [ testPNmspA, testPNmspR, testPNmspExtra, testPNmspF, testPNmspF2 ]

mainList = TestLabel "DataTypePrime" $ 
  TestList [ strTests, intTests, rationalTests, nmspTests ]
main = runTestTT $ TestList [ B.mainList, P.mainList, mainList]
