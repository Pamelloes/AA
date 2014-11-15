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

import Control.Exception
import qualified Data.Map as M
import qualified LZipper_Test as L
import Namespaces
import Opcodes
import qualified Opcodes_Test as O
import Primitives
import qualified Primitives_Test as P
import Test.HUnit
import TestException

-- Namespace Tests
testDefaultNamespace = TestLabel "Test default namespace" $
  TestCase $ assertEqual "" (M.fromList [([],p)]) (defaultNamespace p)
  where p=[F,F,F,F]

testNmspValue = TestLabel "Test value retrieval" $
  TestCase $ assertEqual "" p (nmspValue [] $ defaultNamespace p)
  where p=replicate 5 T

testNmspDefValue = TestLabel "Test default value retrieval" $
  TestCase $ assertEqual "" [] (nmspValue [[F]] $ defaultNamespace p)
  where p=replicate 5 T
  
testNmspValueSet = TestLabel "Test value set" $
  TestCase $ assertEqual "" (M.fromList [([],p),(k,v)])
    (nmspValueSet k v $ defaultNamespace p)
  where p=replicate 10 F
        k=[[F,T],[T,F],[T,T,T,T]]
        v=[F,T,T,T,F]

testNmspValue2 = TestLabel "Test value retrieval 2" $
  TestCase $ assertEqual "" v
    (nmspValue k $ nmspValueSet k v $ defaultNamespace p)
  where p=replicate 5 T
        k=[replicate 4 T,[T,F,T,F],replicate 4 F]
        v=[F,F,F,T,F,F]

testLNmspA = TestLabel "Test loading absolute namespace" $
  TestCase $ assertEqual "" ([],id) (lnmsp [] p)
  where id=[[T,F,T,F],replicate 4 F,replicate 4 T]
        p=(o "AN")++(o "CN")++(o "CS")++id!!0++(o "ES")++(o "CN")
          ++(o "CS")++id!!1++(o "ES")++(o "CN")++(o "CS")++id!!2
          ++(o "ES")++(o "EN")
        o s=opcodes M.! s

testLNmspR = TestLabel "Test loading relative namespace" $
  TestCase $ assertEqual "" ([],id) (lnmsp b p)
  where id=[[T,F,T,F],replicate 4 F,replicate 4 T]
        b=[[T,F,T,F],replicate 4 T]
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
  TestCase $ assertException P.strError (lnmsp [] p)
  where p=(o "AN")++(o "CN")++(o "CS")
        o s=opcodes M.! s

mainList = TestLabel "Namespaces" $
  TestList [ testDefaultNamespace, testNmspValue, testNmspDefValue 
           , testNmspValueSet, testNmspValue2, testLNmspA, testLNmspR
           , testLNmspExtra, testLNmspF, testLNmspF2
           ]
main = runTestTT $ TestList [ L.mainList, O.mainList, P.mainList, mainList ]
