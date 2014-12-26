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
-- This Module contains tests for the Namespaces module.
module Namespaces_Test where

import BitSeries
import qualified BitSeries_Test as B
import Control.Exception
import qualified Data.Map as M
import DataType
import qualified DataType_Test as D
import Opcodes
import qualified Opcodes_Test as O
import Test.HUnit
import TestException

-- Utilities
maid :: ANmsp -> DataType
maid x = ([],BNmspId $ Left x)

mrid :: RNmsp -> DataType
mrid x = ([],BNmspId $ Right x)

tT :: BitSeries -> BitSeries
tT (Terminate:as)=[Terminate]
tT (a:as)=a:(tT as)

testMaid = TestLabel "Verify maid" $
  TestCase $ assertEqual "" ([],BNmspId$Left p) (maid p)
  where p=[[F,T,F,T],[T,T,T,T],[F,F,F,F]]

testMrid = TestLabel "Verify mrid" $
  TestCase $ assertEqual "" ([],BNmspId$Right p) (mrid p)
  where p=[Parent,Parent,Child [F,F,F,F],Child [T,T,T,T]]

testTt = TestLabel "Verify tT" $
  TestCase $ assertEqual "" [T,T,F,F,Terminate] (tT $ [T,T,F,F,Terminate,F,T,F]
  ++ (repeat Terminate))

utilList = TestLabel "Namespaces_Test Utilities" $
  TestList [ testMaid, testMrid, testTt
           ]

-- Namespace Tests
testDefaultNamespace = TestLabel "Test default namespace" $
  TestCase $ assertEqual "" (M.fromList [([],(p,BStatement))]) (defaultNamespace p)
  where p=[F,F,F,F]

testANmspValue = TestLabel "Test absolute value retrieval" $
  TestCase $ assertEqual "" (p,BStatement) (nmspValue [] (maid []) $ defaultNamespace p)
  where p=replicate 5 T

testRNmspValue = TestLabel "Test relative value retrieval" $
  TestCase $ assertEqual "" (p,BStatement) (nmspValue [[T,T,T,T]] (mrid [Parent]) $ defaultNamespace p)
  where p=[T,F,F,F,T,F,T,T,F,Terminate]

testANmspDefValue = TestLabel "Test absolute default value retrieval" $
  TestCase $ assertEqual "" ([Terminate],BString []) (tT a,b)
  where p=replicate 5 T
        (a,b)=(nmspValue [] (maid [[F,F,F,F]]) $ defaultNamespace p)

testRNmspDefValue = TestLabel "Test relative default value retrieval" $
  TestCase $ assertEqual "" ([Terminate],BString []) (tT a,b)
  where p=replicate 5 F
        (a,b)=(nmspValue [[T,F,T,F]] (mrid [Parent,Child [F,F,F,F]]) $ defaultNamespace p)
  
testANmspValueSet = TestLabel "Test absolute value set" $
  TestCase $ assertEqual "" (M.fromList [([],(p,BStatement)),(k,v)])
    (nmspValueSet [] (maid k) v $ defaultNamespace p)
  where p=replicate 10 F
        k=[[T,T,T,T],[F,T,T,F]]
        v=((opcodes M.! "CS")++[F,T,T,T]++(opcodes M.! "ES"),BString [F,T,T,T])

testRNmspValueSet = TestLabel "Test relative value set" $
  TestCase $ assertEqual "" (M.fromList [([],(p,BStatement)),(k2,v)])
    (nmspValueSet [[F,F,F,F]] (mrid k) v $ defaultNamespace p)
  where p=replicate 10 F
        k=[Parent, Child [T,T,T,T],Child [F,T,T,F]]
        k2=[[T,T,T,T],[F,T,T,F]]
        v=((opcodes M.! "CS")++[T,T,F,T]++(opcodes M.! "ES"),BString [T,T,F,T])

testANmspValue2 = TestLabel "Test absolute value retrieval 2" $
  TestCase $ assertEqual "" v
  (nmspValue [] (maid k) $ nmspValueSet [] (maid k) v $ defaultNamespace p)
  where p=replicate 5 T
        k=[[F,T,F,T],[T,F,T,F],replicate 4 Terminate]
        v=([F,F,T,T,F,F,T,T,T],BStatement)

testRNmspValue2 = TestLabel "Test relative value retrieval 2" $
  TestCase $ assertEqual "" v
  (nmspValue [] (maid k) $ nmspValueSet k2 (mrid k3) v $ defaultNamespace p)
  where p=replicate 5 T
        k=[[F,T,F,T],[T,F,T,F],replicate 4 Terminate]
        k2=[[F,T,F,T],[T,T,T,T],[T,T,T,T]]
        k3=[Parent,Parent,Child [T,F,T,F],Child $ replicate 4 Terminate]
        v=([T,F,T,F,F,T,F],BStatement)

testFTEq = TestLabel "Test False/Terminate equivalence" $
  TestCase $ assertEqual "" v
  (nmspValue [] (maid k) $ nmspValueSet [] (maid k2) v $ defaultNamespace p)
  where p=replicate 5 T
        k=[[F,T,F,T],[T,F,T,F],replicate 4 Terminate]
        k2=[[F,T,F,T],[T,F,T,F],replicate 4 F]
        v=([F,F,T,T,F,F,T,T,T],BStatement)

{-
testLNmspA = TestLabel "Test loading absolute namespace" $
  TestCase $ assertEqual "" ([],fmap BString id) (lnmsp [] p)
  where id=[[T,F,T,F],replicate 4 F,replicate 4 T]
        p=(o "AN")++(o "CN")++(o "CS")++id!!0++(o "ES")++(o "CN")
          ++(o "CS")++id!!1++(o "ES")++(o "CN")++(o "CS")++id!!2
          ++(o "ES")++(o "EN")
        o s=opcodes M.! s

testLNmspR = TestLabel "Test loading relative namespace" $
  TestCase $ assertEqual "" ([],fmap BString id) (lnmsp b p)
  where id=[[T,F,T,F],replicate 4 F,replicate 4 T]
        b=[BString [T,F,T,F],BString $ replicate 4 T]
        p=(o "RN")++(o "PN")++(o "CN")++(o "CS")++id!!1++(o "ES")
          ++(o "CN")++(o "CS")++id!!2++(o "ES")++(o "ERN")
        o s=opcodes M.! s

testLNmspExtra = TestLabel "Test loading namespace with extra program" $
  TestCase $ assertEqual "" (e,id) (lnmsp [] (p++e))
    where id=[]
          p=(o "AN")++(o "EN")
          e=replicate 103 F 
          o s=opcodes M.! s

testLNmspF = TestLabel "Test loading short namespace" $
  TestCase $ assertException O.opError (lnmsp [] [])

testLNmspF2 = TestLabel "Test loading short namespace 2" $
  TestCase $ assertException D.strError (lnmsp [] p)
  where p=(o "AN")++(o "CN")++(o "CS")
        o s=opcodes M.! s
-}
mainList = TestLabel "Namespaces" $
  TestList [ utilList, testDefaultNamespace, testANmspValue, testRNmspValue
           , testANmspDefValue, testRNmspDefValue, testANmspValueSet
           , testRNmspValueSet, testANmspValue2, testRNmspValue2, testFTEq
           {-, testLNmspA, testLNmspR
           , testLNmspExtra, testLNmspF, testLNmspF2-}
           ]
main = runTestTT $ TestList [ B.mainList, O.mainList, D.mainList, mainList ]
