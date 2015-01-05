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
-- This module provides tests for the Statement module
module Statement_Test where

import BitSeries
import qualified BitSeries_Test as B
import Control.Exception
import qualified Data.Map as M
import DataType
import qualified DataType_Test as D
import qualified Namespaces_Test as N
import Opcodes
import qualified Opcodes_Test as O
import Statement
import Test.HUnit
import TestException

instance (Eq a) => Eq (Stmt a) where
  (LS a)==(LS b)=a==b
  (AS a b)==(AS c d)=(a==c)&&(b==d)
  (RS a)==(RS b)=a==b
  (ET a)==(ET b)=a==b
  (SQ a b)==(SQ c d)=(a==c)&&(b==d)
  (IF a b c)==(IF d e f)=(a==d)&&(b==e)&&(c==f)
  (DW a b)==(DW c d)=(a==c)&&(b==d)
  (MSA a b)==(MSA c d)=(a==c)&&(b==d)
  (MSB a b c)==(MSB d e f)=(a==d)&&(b==e)&&(c==f)
  (IOS a)==(IOS b)=(a==b)
  _==_=False

-- Literal Statement Tests
testLsS = TestLabel "Test loading literal string" $
  TestCase $ assertEqual "" ([],((p,BStatement 0),Free (LS (p1, BString s))))
    (loadLS 0 p)
  where s=[T,T,F,F]
        p1=(o "CS")++s++(o "ES")
        p=(o "LT")++p1
        o t=opcodes M.! t

testLsSE = TestLabel "Test loading literal string with extra" $
  TestCase $ assertEqual "" (p2,((p,BStatement 0),Free (LS (p1, BString s))))
    (loadLS 0 $ p++p2)
  where s=[T,T,F,F]
        p1=(o "CS")++s++(o "ES")
        p2=replicate 10 T
        p=(o "LT")++p1
        o t=opcodes M.! t

testLsI = TestLabel "Test loading literal integer" $
  TestCase $ assertEqual "" ([],((p,BStatement 0),Free (LS (p1, BInteger 9))))
    (loadLS 0 p)
  where p1=[F]++(o "CS")++[T,F,F,T]++(o "ES")
        p=(o "LI")++p1
        o t=opcodes M.! t

testLsR = TestLabel "Test loading literal rational" $
  TestCase $ assertEqual "" ([],((p,BStatement 0),Free (LS (p1, BRational 2 3))))
    (loadLS 0 p)
  where p1= [F]++(o "CS")++[F,F,T,F]++(o "ES")
          ++[F]++(o "CS")++[F,F,T,T]++(o "ES")
        p=(o "LR")++p1
        o t=opcodes M.! t

testLsN = TestLabel "Test loading literal namespace" $
  TestCase $ assertEqual "" ([],((p,BStatement 0),Free (LS (p1, BNmspId $ Left []))))
    (loadLS 0 p)
  where p1=(o "AN")++(o "EN")
        p=(o "LN")++p1
        o t=opcodes M.! t

testLsM = TestLabel "Test loading literal statement" $
  TestCase $ assertEqual "" ([],((p,BStatement 0),Free (LS (p1, BStatement 1))))
    (loadLS 0 p)
  where p1a=[F]++(o "CS")++[F,F,F,T]++(o "ES")
        p1b=(o "LS")++(o "LI")++[T]++(o "ES")
        p1=p1a++p1b
        p=(o "LM")++p1
        o t=opcodes M.! t

testLsME = TestLabel "Test loading literal statement with extra" $
  TestCase $ assertEqual "" (p2,((p,BStatement 0),Free (LS (p1, BStatement 1))))
    (loadLS 0 $ p++p2)
  where p1a=[F]++(o "CS")++[F,F,F,T]++(o "ES")
        p1b=(o "LS")++(o "LI")++[T]++(o "ES")
        p1=p1a++p1b
        p2=[T,F,F,F,Terminate,Terminate,F,T,F]
        p=(o "LM")++p1
        o t=opcodes M.! t

-- I/O Statement Tests
testIO = TestLabel "Test loading i/o statement" $
  TestCase $ assertEqual "" ([],((p,BStatement 0),r)) (loadIO 0 p)
  where r=Free (IOS (Free (LS (q, BInteger (-4)))))
        q=[T]++(o "CS")++[T,T,F,F]++(o "ES")
        p=(o "LS")++(o "LI")++q
        o t=opcodes M.! t

testIOE = TestLabel "Test loading i/o statement with extra" $
  TestCase $ assertEqual "" (p2,((p,BStatement 0),r)) (loadIO 0 $ p++p2)
  where r=Free (IOS (Free (LS (q, BString s))))
        s=[T,T,F,F]
        q=(o "CS")++s++(o "ES")
        p=(o "LS")++(o "LT")++q
        p2=[F,F,F,T,T,T,F,T,F,F,T,Terminate]
        o t=opcodes M.! t


-- Load Statement Tests
testLtS = TestLabel "Test loading embedded literal string" $
  TestCase $ assertEqual "" ([],((p,BStatement 0),Free (LS (p1, BString s))))
    (loadStmt 0 p)
  where s=[T,T,F,F]
        p1=(o "CS")++s++(o "ES")
        p=(o "LS")++(o "LT")++p1
        o t=opcodes M.! t

testLtRE = TestLabel "Test loading embedded literal rational with extra" $
  TestCase $ assertEqual "" (p2,((p,BStatement 0),Free (LS (p1, BRational 0 0))))
    (loadStmt 0 $ p++p2)
  where p1=[T]++(o "CS")++[F,F,T,T]++(o "ES")++[F]++(o "ES")
        p2=replicate 10 T
        p=(o "LS")++(o "LR")++p1
        o t=opcodes M.! t

testLtI = TestLabel "Test loading embedded i/o statement" $
  TestCase $ assertEqual "" ([],((p,BStatement 0),r)) (loadStmt 0 p)
  where r=Free (IOS (Free (LS (q, BRational 7 (-9)))))
        q=[F]++(o "CS")++[F,T,T,T]++(o "ES")++[T]++(o "CS")++[F,T,T,T]++(o "ES")
        p=(o "IO")++(o "LS")++(o "LR")++q
        o t=opcodes M.! t

testLtIE = TestLabel "Test loading embedded i/o statement with extra" $
  TestCase $ assertEqual "" (p2,((p,BStatement 0),r)) (loadStmt 0 $ p++p2)
  where r=Free (IOS (Free (LS (q, BNmspId $ Right [Parent,Child [F,T,F,T]]))))
        q=(o "RN")++(o "PN")++(o "CN")++(o "CS")++[F,T,F,T]++(o "ES")++(o "ERN")
        p=(o "IO")++(o "LS")++(o "LN")++q
        p2=[F,F,F,T,T,T,F,T,F,F,T,Terminate]
        o t=opcodes M.! t

mainList = TestLabel "Statements" $
  TestList [ testLsS, testLsSE, testLsI, testLsR, testLsN, testLsM, testLsME
           , testIO, testIOE, testLtS, testLtRE, testLtI, testLtIE
           ]

main = runTestTT $
  TestList [ B.mainList, O.mainList, D.mainList, N.mainList, mainList ]
