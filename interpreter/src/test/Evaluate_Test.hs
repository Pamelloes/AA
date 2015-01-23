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
-- This module provides tests for the Evaluate module
module Evaluate_Test where

import BitSeries
import qualified BitSeries_Test as B
import Control.Exception
import qualified Data.Map as M
import DataType
import qualified DataType_Test as D
import DataType.Util
import qualified DataType_Util_Test as DU
import Evaluate
import Opcodes
import qualified Opcodes_Test as O
import Statement
import qualified Statement_Test as S
import Test.HUnit
import TestException

-- Utilities
maid :: ANmsp -> DataType
maid x = ([],BNmspId $ Left x)

mrid :: RNmsp -> DataType
mrid x = ([],BNmspId $ Right x)

testMaid = TestLabel "Verify maid" $
  TestCase $ assertEqual "" ([],BNmspId$Left p) (maid p)
  where p=[[F,T,F,T],[T,T,T,T],[F,F,F,F]]

testMrid = TestLabel "Verify mrid" $
  TestCase $ assertEqual "" ([],BNmspId$Right p) (mrid p)
  where p=[Parent,Parent,Child [F,F,F,F],Child [T,T,T,T]]


utilList = TestLabel "Namespaces_Test Utilities" $
  TestList [ testMaid, testMrid ]

-- Namespace Functionality Tests
testDefaultNamespace = TestLabel "Test default namespace" $
  TestCase $ assertEqual "" (M.fromList [([],(p,BStatement))]) (defaultNamespace p)
  where p=[F]++(opcodes M.! "ES")++[F,F,F,F]

testANmspValue = TestLabel "Test absolute value retrieval" $
  TestCase $ assertEqual "" (p,BStatement) (nmspValue [] (maid []) $ defaultNamespace p)
  where p=[F]++(opcodes M.! "ES")++replicate 5 T

testRNmspValue = TestLabel "Test relative value retrieval" $
  TestCase $ assertEqual "" (p,BStatement) (nmspValue [[T,T,T,T]] (mrid [Parent]) $ defaultNamespace p)
  where p=[F]++(opcodes M.! "ES")++[T,F,F,F,T,F,T,T,F]

testANmspDefValue = TestLabel "Test absolute default value retrieval" $
  TestCase $ assertEqual "" ([F],BString []) (a,b)
  where p=[F]++(opcodes M.! "ES")++replicate 5 T
        (a,b)=(nmspValue [] (maid [[F,F,F,F]]) $ defaultNamespace p)

testRNmspDefValue = TestLabel "Test relative default value retrieval" $
  TestCase $ assertEqual "" ([F],BString []) (a,b)
  where p=[F]++(opcodes M.! "ES")++replicate 5 F
        (a,b)=(nmspValue [[T,F,T,F]] (mrid [Parent,Child [F,F,F,F]]) $ defaultNamespace p)
  
testANmspValueSet = TestLabel "Test absolute value set" $
  TestCase $ assertEqual "" (M.fromList [([],(p,BStatement)),(k,v)])
    (nmspValueSet [] (maid k) v $ defaultNamespace p)
  where p=[F]++(opcodes M.! "ES")++replicate 10 F
        k=[[T,T,T,T],[F,T,T,F]]
        v=((opcodes M.! "CS")++[F,T,T,T]++(opcodes M.! "ES"),BString [F,T,T,T])

testRNmspValueSet = TestLabel "Test relative value set" $
  TestCase $ assertEqual "" (M.fromList [([],(p,BStatement)),(k2,v)])
    (nmspValueSet [[F,F,F,F]] (mrid k) v $ defaultNamespace p)
  where p=[F]++(opcodes M.! "ES")++replicate 10 F
        k=[Parent, Child [T,T,T,T],Child [F,T,T,F]]
        k2=[[T,T,T,T],[F,T,T,F]]
        v=((opcodes M.! "CS")++[T,T,F,T]++(opcodes M.! "ES"),BString [T,T,F,T])

testANmspValue2 = TestLabel "Test absolute value retrieval 2" $
  TestCase $ assertEqual "" v
  (nmspValue [] (maid k) $ nmspValueSet [] (maid k) v $ defaultNamespace p)
  where p=[F]++(opcodes M.! "ES")++replicate 5 T
        k=[[F,T,F,T],[T,F,T,F],replicate 4 F]
        v=([F,F,T,T,F,F,T,T,T],BStatement)

testRNmspValue2 = TestLabel "Test relative value retrieval 2" $
  TestCase $ assertEqual "" v
  (nmspValue [] (maid k) $ nmspValueSet k2 (mrid k3) v $ defaultNamespace p)
  where p=[F]++(opcodes M.! "ES")++replicate 5 T
        k=[[F,T,F,T],[T,F,T,F],replicate 4 F]
        k2=[[F,T,F,T],[T,T,T,T],[T,T,T,T]]
        k3=[Parent,Parent,Child [T,F,T,F],Child $ replicate 4 F]
        v=([T,F,T,F,F,T,F],BStatement)

testFTEq = TestLabel "Test False/Terminate equivalence" $
  TestCase $ assertEqual "" v
  (nmspValue [] (maid k) $ nmspValueSet [] (maid k2) v $ defaultNamespace p)
  where p=[F]++(opcodes M.! "ES")++replicate 5 T
        k=[[F,T,F,T],[T,F,T,F],replicate 4 F]
        k2=[[F,T,F,T],[T,F,T,F],replicate 4 F]
        v=([F,F,T,T,F,F,T,T,T],BStatement)

-- Utility Tests
-- Operations Tests
testPrior0 = TestLabel "Test priority 0" $
  TestCase $ assertEqual "" 0 (prior BStatement)
testPrior1 = TestLabel "Test priority 1" $
  TestCase $ assertEqual "" 1 (prior $ BNmspId $ Left [])
testPrior2 = TestLabel "Test priority 2" $
  TestCase $ assertEqual "" 2 (prior $ BString [])
testPrior3 = TestLabel "Test priority 3" $
  TestCase $ assertEqual "" 3 (prior $ BInteger 0)
testPrior4 = TestLabel "Test priority 4" $
  TestCase $ assertEqual "" 4 (prior $ BRational 0 0)

mainList = TestLabel "Evaluate" $
  TestList [ utilList, testDefaultNamespace, testANmspValue, testRNmspValue
           , testANmspDefValue, testRNmspDefValue, testANmspValueSet
           , testRNmspValueSet, testANmspValue2, testRNmspValue2, testFTEq
           , testPrior0, testPrior1, testPrior2, testPrior3, testPrior4
           ]

main = runTestTT $
  TestList [ B.mainList, O.mainList, D.mainList, S.mainList, DU.mainList, mainList ]
