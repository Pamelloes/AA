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
-- This module profides tests for the Statement module
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

{-
instance (Eq a, Eq b) => Eq (Free a b)
instance (Eq a) => Eq (Stmt a)
--instance Eq (Stmt a)

-- Literal Statement Tests
testLsS = TestLabel "Test loading literal string" $
  TestCase $ assertEqual "" 
    (([],Free (LS (p,BString s)) $ Pure ())::(Eq a,Show a)=>(BitSeries,Free
    (Stmt a) ()))
    ((loadLS p)::(Eq a,Show a)=>(BitSeries,Free (Stmt a) ()))
  where s=[T,T,F,F]
        p=(o "LT")++(o "CS")++s++(o "ES")
        o t=opcodes M.! t
-}

mainList = TestLabel "Statements" $
  TestList [] -- testLsS ]

main = runTestTT $
  TestList [ B.mainList, O.mainList, D.mainList, N.mainList, mainList ]
