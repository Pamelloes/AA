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
import Opcodes
import qualified Opcodes_Test as O
import Statement
import Test.HUnit
import TestException
import TestUtil
import Text.Parsec.Prim

instance (Eq a) => Eq (Stmt a) where
  (AS a b)==(AS c d)=(a==c)&&(b==d)
  (RS a)==(RS b)=a==b
  (ET a b)==(ET c d)=(a==c)&&(b==d)
  (SQ a b)==(SQ c d)=(a==c)&&(b==d)
  (IF a b c)==(IF d e f)=(a==d)&&(b==e)&&(c==f)
  (DW a b)==(DW c d)=(a==c)&&(b==d)
  (MSA a b)==(MSA c d)=(a==c)&&(b==d)
  (MSB a b c)==(MSB d e f)=(a==d)&&(b==e)&&(c==f)
  (IOS a)==(IOS b)=(a==b)
  _==_=False

-- DStmt and (BitSeries,DStmt) Terminate Truncators
{-
tDS :: DStmt -> DStmt
tDS (d,f) = (D.tD d,fmap D.tD f)
tBD :: (BitSeries,DStmt) -> (BitSeries,DStmt)
tBD (b,d) = (B.tT b,tDS d)
-}

ptest' :: String -> DStmt -> Parsec BitSeries () DStmt -> BitSeries -> Test
ptest' s a p b = etest s (Right a) $ {-fmap (tDS)-} (parse p "" b)

-- Literal Statement Tests
testLsS = ptest' "Test loading literal string" 
  ((p,BStatement),Pure (p1', BString s)) loadLS p
  where s=[T,T,F,F]
        p1=(o "CS")++s++(o "ES")
        p1'=p1++[{-Terminate-}]
        p=(o "LT")++p1

testLsSE = ptest' "Test loading literal string with extra"
 ((p,BStatement),Pure (p1', BString s)) (loadLS>>loadLS) (p2++p)
  where s=[T,T,F,F]
        p1=(o "CS")++s++(o "ES")
        p1'=p1++[{-Terminate-}]
        p2=(o "LT")++(o "CS")++[T,F,T,F]++(o "ES")
        p=(o "LT")++p1

testLsI = ptest' "Test loading literal integer" 
  ((p,BStatement),Pure (p1', BInteger 9)) loadLS p
  where p1=[F]++(o "CS")++[T,F,F,T]++(o "ES")
        p1'=p1++[{-Terminate-}]
        p=(o "LI")++p1

testLsR = ptest' "Test loading literal rational"
  ((p,BStatement),Pure (p1', BRational 2 3)) loadLS p
  where p1= [F]++(o "CS")++[F,F,T,F]++(o "ES")
          ++[F]++(o "CS")++[F,F,T,T]++(o "ES")
        p1'=p1++[{-Terminate-}]
        p=(o "LR")++p1

testLsN = ptest' "Test loading literal namespace"
  ((p,BStatement),Pure (p1', BNmspId $ Left [])) loadLS p
  where p1=(o "AN")++(o "EN")
        p1'=p1++[{-Terminate-}]
        p=(o "LN")++p1

testLsM = ptest' "Test loading literal statement" 
  ((p,BStatement),Pure (p1', BStatement)) loadLS p
  where p1=(o "LS")++(o "LI")++[T]++(o "ES")
        p1'=p1++[{-Terminate-}]
        p=(o "LM")++p1

testLsME = ptest' "Test loading literal statement with extra"
  ((p,BStatement),Pure (p1', BStatement)) (loadLS>>loadLS) (p2++p)
  where p1=(o "LS")++(o "LI")++[T]++(o "ES")
        p1'=p1++[{-Terminate-}]
        p2=(o "LM")++(o "LS")++(o "LT")++(o "ES")
        p=(o "LM")++p1

-- Control Statement Test
testCtA = ptest' "Test loading assign statement"
  ((p,BStatement),r) loadTS p
  where r=Free (AS (Pure (q', BInteger (-4))) (Pure (q2',BString s)))
        q=[T]++(o "CS")++[T,T,F,F]++(o "ES")
        q'=q++[{-Terminate-}]
        s=[T,F,F,T]
        q2=(o "CS")++s++(o "ES")
        q2'=q2++[{-Terminate-}]
        p=(o "AS")++(o "LS")++(o "LI")++q++(o "LS")++(o "LT")++q2

testCtAE = ptest' "Test loading assign statement with extra"
  ((p,BStatement),r) (loadTS>>loadTS) (p2++p)
  where r=Free (AS (Pure (q',BNmspId $ Left [[T,T,T,T]]))
                   (Pure (q2', BString [T,F,T,F])))
        q=(o "AN")++(o "CN")++(o "CS")++[T,T,T,T]++(o "ES")++(o "EN")
        q'=q++[{-Terminate-}]
        q2=(o "CS")++[T,F,T,F]++(o "ES")
        q2'=q2++[{-Terminate-}]
        p=(o "AS")++(o "LS")++(o "LN")++q++(o "LS")++(o "LT")++q2
        p2= (o "AS")++(o "LS")++(o "LI")++[F]++(o "CS")++[T,T,T,T]++(o "ES")
          ++(o "LS")++(o "LS")++(o "ES")

testCtR = ptest' "Test loading retrieve statement"
  ((p,BStatement),r) loadTS p
  where r=Free (RS (Pure (q',BString s)))
        s=[T,T,T,T]
        q=(o "CS")++s++(o "ES")
        q'=q++[{-Terminate-}]
        p=(o "RS")++(o "LS")++(o "LT")++q

testCtRE = ptest' "Test loading retrieve statement with extra"
  ((p, BStatement),r) (loadTS>>loadTS) (p2++p)
  where r=Free (RS (Pure (q', BNmspId $ Right [Parent,Child [T,F,F,F]])))
        q=(o "RN")++(o "PN")++(o "CN")++(o "CS")++[T,F,F,F]++(o "ES")++(o "ERN")
        q'=q++[{-Terminate-}]
        p=(o "RS")++(o "LS")++(o "LN")++q
        p2=(o "RS")++(o "LS")++(o "LR")++[F]++(o "ES")++[T]++(o "ES")

testCtS = ptest' "Test loading sequence statement"
  ((p,BStatement),r) loadTS p
  where r=Free (SQ (Pure (q', BRational 4 (-3))) (Pure (q2', BStatement)))
        q=[F]++(o "CS")++[F,T,F,F]++(o "ES")++[T]++(o "CS")++[T,T,F,T]++(o "ES")
        q'=q++[{-Terminate-}]
        q2=(o "LS")++(o "LT")++(o "CS")++[T,T,T,F]++(o "ES")
        q2'=q2++[{-Terminate-}]
        p=(o "SQ")++(o "LS")++(o "LR")++q++(o "LS")++(o "LM")++q2

testCtSE = ptest' "Test loading sequence statement with extra"
  ((p,BStatement),r) (loadTS>>loadTS) (p2++p)
  where r=Free (SQ (Pure (q', BNmspId $ Left []))
                   (Pure (q2', BString [T,F,T,T])))
        q=(o "AN")++(o "EN")
        q'=q++[{-Terminate-}]
        q2=(o "CS")++[T,F,T,T]++(o "ES")
        q2'=q2++[{-Terminate-}]
        p=(o "SQ")++(o "LS")++(o "LN")++q++(o "LS")++(o "LT")++q2
        p2= (o "SQ")++(o "LS")++(o "LT")++(o "ES")++(o "LS")++(o "LI")++[T]
          ++(o "CS")++[F,F,T,F]++(o "ES")

testCtI = ptest' "Test loading if statement" 
  ((p,BStatement),r) loadTS p
  where r=Free (IF (Pure (q', BNmspId $ Left []))
                   (Pure (q2', BRational (-12) 13))
                   (Pure (q3', BStatement)))
        q=(o "AN")++(o "EN")
        q'=q++[{-Terminate-}]
        q2= [T]++(o "CS")++[F,T,F,F]++(o "ES")++[F]++(o "CS")++[T,T,F,T]
          ++(o "ES")
        q2'=q2++[{-Terminate-}]
        q3=(o "LS")++(o "LN")++(o "RN")++(o "PN")++(o "ERN")
        q3'=q3++[{-Terminate-}]
        p= (o "IF")++(o "LS")++(o "LN")++q++(o "LS")++(o "LR")++q2
         ++(o "LS")++(o "LM")++q3

testCtIE = ptest' "Test loading if statement with extra"
  ((p,BStatement),r) (loadTS>>loadTS) (p2++p)
  where r=Free (IF (Pure (q', BNmspId $ Left []))
                   (Pure (q2', BStatement))
                   (Pure (q3', BRational (-10) 10)))
        q=(o "AN")++(o "EN")
        q'=q++[{-Terminate-}]
        q2= (o "FS")++(o "MS")++(o "BN")++(o "LS")++(o "LN")++(o "RN")++(o "PN")
          ++(o "ERN")
        q2'=q2++[{-Terminate-}]
        q3= [T]++(o "CS")++[F,T,T,F]++(o "ES")++[F]++(o "CS")++[T,F,T,F]
          ++(o "ES")
        q3'=q3++[{-Terminate-}]
        p= (o "IF")++(o "LS")++(o "LN")++q++(o "LS")++(o "LM")++q2
         ++(o "LS")++(o "LR")++q3
        p2= (o "IF")++(o "LS")++(o "LT")++(o "ES")++(o "LS")++(o "LN")++(o "RN")
          ++(o "PN")++(o "CN")++(o "ES")++(o "ERN")++(o "LS")++(o "LI")++[F]
          ++(o "CS")++[T,F,T,F]++(o "ES")

testCtD = ptest' "Test loading do while statement" 
  ((p,BStatement),r) loadTS p
  where r=Free (DW (Pure (q', BRational 5 (-1))) (Pure (q2', BStatement)))
        q=[F]++(o "CS")++[F,T,F,T]++(o "ES")++[T]++(o "CS")++[T,T,T,T]++(o "ES")
        q'=q++[{-Terminate-}]
        q2=(o "LS")++(o "LT")++(o "CS")++[F,T,T,F]++(o "ES")
        q2'=q2++[{-Terminate-}]
        p=(o "DW")++(o "LS")++(o "LR")++q++(o "LS")++(o "LM")++q2

testCtDE = ptest' "Test loading do while statement with extra"
  ((p,BStatement),r) (loadTS>>loadTS) (p2++p)
  where r=Free (DW (Pure (q', BNmspId $ Right []))
                   (Pure (q2', BString [F,T,T,T])))
        q=(o "RN")++(o "ERN")
        q'=q++[{-Terminate-}]
        q2=(o "CS")++[F,T,T,T]++(o "ES")
        q2'=q2++[{-Terminate-}]
        p=(o "DW")++(o "LS")++(o "LN")++q++(o "LS")++(o "LT")++q2
        p2= (o "DW")++(o "LS")++(o "LT")++(o "ES")++(o "LS")++(o "LN")++(o "AN")
          ++(o "EN")

testCtE0 = ptest' "Test loading execute statement - 0 parameters"
  ((p,BStatement),r) loadTS p
  where r=Free (ET (Pure (q', BRational 15 (-1))) [])
        q=[F]++(o "CS")++[T,T,T,T]++(o "ES")++[T]++(o "CS")++[T,T,T,T]++(o "ES")
        q'=q++[{-Terminate-}]
        i=[F]++(o "ES")
        p=(o "ET")++(o "LS")++(o "LR")++q++i

testCtE1 = ptest' "Test loading execute statement - 1 parameters"
  ((p,BStatement),r) loadTS p
  where r=Free (ET (Pure (q', BRational 15 (-1))) [
           Pure (q1',BStatement)
           ])
        q=[F]++(o "CS")++[T,T,T,T]++(o "ES")++[T]++(o "CS")++[T,T,T,T]++(o "ES")
        q'=q++[{-Terminate-}]
        i=[T]++(o "CS")++[T,T,T,T]++(o "ES")
        q1=(o "LS")++(o "LT")++(o "ES")
        q1'=q1++[{-Terminate-}]
        p= (o "ET")++(o "LS")++(o "LR")++q++i
         ++(o "LS")++(o "LM")++q1

testCtE4 = ptest' "Test loading execute statement - 4 parameters" 
  ((p,BStatement),r) loadTS p
  where r=Free (ET (Pure (q', BRational 15 (-1))) [
           Pure (q1',BStatement),
           Free (MSB "OP" (Pure (q2a',BString []))
                          (Pure (q2b',BString [T,T,T,T,F,F,F,F]))),
           Pure (q3',BStatement),
           Pure (q4',BNmspId $ Right [Parent,Parent,Child [T,T,F,F]])
           ])
        q=[F]++(o "CS")++[T,T,T,T]++(o "ES")++[T]++(o "CS")++[T,T,T,T]++(o "ES")
        q'=q++[{-Terminate-}]
        i=[T]++(o "CS")++[T,T,F,F]++(o "ES")
        q1=(o "LS")++(o "LT")++(o "ES")
        q1'=q1++[{-Terminate-}]
        q2a=(o "ES")
        q2a'=q2a++[{-Terminate-}]
        q2b=(o "CS")++[T,T,T,T]++(o "CS")++[F,F,F,F]++(o "ES")
        q2b'=q2b++[{-Terminate-}]
        q3=(o "LS")++(o "LN")++(o "AN")++(o "EN")
        q3'=q3++[{-Terminate-}]
        q4= (o "RN")++(o "PN")++(o "PN")++(o "CN")++(o "CS")++[T,T,F,F]
          ++(o "ES")++(o "ERN")
        q4'=q4++[{-Terminate-}]
        p= (o "ET")++(o "LS")++(o "LR")++q++i
         ++(o "LS")++(o "LM")++q1
         ++(o "FS")++(o "MS")++(o "OP")++(o "LS")++(o "LT")++q2a++(o "LS")
         ++(o "LT")++q2b
         ++(o "LS")++(o "LM")++q3
         ++(o "LS")++(o "LN")++q4

testCtE1E = ptest' "Test loading execute statement - 1 parameters with extra"
  ((p,BStatement),r) (loadTS>>loadTS) (p2++p)
  where r=Free (ET (Pure (q', BRational 15 (-1))) [
           Pure (q1',BStatement)
           ])
        q=[F]++(o "CS")++[T,T,T,T]++(o "ES")++[T]++(o "CS")++[T,T,T,T]++(o "ES")
        q'=q++[{-Terminate-}]
        i=[T]++(o "CS")++[T,T,T,T]++(o "ES")
        q1=(o "LS")++(o "LN")++(o "RN")++(o "ERN")
        q1'=q1++[{-Terminate-}]
        p= (o "ET")++(o "LS")++(o "LR")++q++i
         ++(o "LS")++(o "LM")++q1
        p2= (o "ET")++(o "LS")++(o "LN")++(o "RN")++(o "ERN")++[F]++(o "CS")
          ++[F,F,F,T]++(o "ES")++(o "LS")++(o "LI")++[F]++(o "ES")

-- Math Statement Test
testMA = ptest' "Test loading 1-var mathematical statement"
  ((p,BStatement),r) loadMS p
  where r=Free (MSA "BN" (Pure (q', BInteger (-4))))
        q=[T]++(o "CS")++[T,T,F,F]++(o "ES")
        q'=q++[{-Terminate-}]
        p=(o "BN")++(o "LS")++(o "LI")++q

testMAE = ptest' "Test loading 1-var mathematical statement with extra"
  ((p,BStatement),r) (loadMS>>loadMS) (p2++p)
  where r=Free (MSA "TN" (Pure (q', BString [T,T,T,F])))
        q=(o "CS")++[T,T,T,F]++(o "ES")
        q'=q++[{-Terminate-}]
        p=(o "TN")++(o "LS")++(o "LT")++q
        p2=(o "TN")++(o "LS")++(o "LI")++[T]++(o "ES")

testMB = ptest' "Test loading 2-var mathematical statement"
  ((p,BStatement),r) loadMS p
  where r=Free (MSB "BGE" (Pure (q', BInteger 12)) 
                         (Pure (q2', BString [])))
        q=[F]++(o "CS")++[T,T,F,F]++(o "ES")
        q'=q++[{-Terminate-}]
        q2=(o "ES")
        q2'=q2++[{-Terminate-}]
        p=(o "BGE")++(o "LS")++(o "LI")++q++(o "LS")++(o "LT")++q2

testMBE = ptest' "Test loading 2-var mathematical statement with extra"
  ((p,BStatement),r) (loadMS>>loadMS) (p2++p)
  where r=Free (MSB "TR" (Pure (q', BStatement)) 
                         (Pure (q2', BNmspId $ Left $ [[F,F,T,F]])))
        q=(o "LS")++(o "LT")++(o "ES")
        q'=q++[{-Terminate-}]
        q2=(o "AN")++(o "CN")++(o "CS")++[F,F,T,F]++(o "ES")++(o "EN")
        q2'=q2++[{-Terminate-}]
        p=(o "TR")++(o "LS")++(o "LM")++q++(o "LS")++(o "LN")++q2
        p2= (o "OP")++(o "LS")++(o "LI")++[F]++(o "CS")++[F,F,F,T]++(o "ES")
          ++(o "LS")++(o "LI")++[F]++(o "CS")++[F,F,T,F]++(o "ES")

-- Functional Statement Tests
testTsE1 = ptest' "Test loading functional execute statement - 1 parameters"
  ((p,BStatement),r) loadFS p
  where r=Free (ET (Pure (q', BRational 15 (-1))) [
           Pure (q1',BString [T,T,F,F])
           ])
        q=[F]++(o "CS")++[T,T,T,T]++(o "ES")++[T]++(o "CS")++[T,T,T,T]++(o "ES")
        q'=q++[{-Terminate-}]
        i=[T]++(o "CS")++[T,T,T,T]++(o "ES")
        q1=(o "CS")++[T,T,F,F]++(o "ES")
        q1'=q1++[{-Terminate-}]
        p= (o "TS")++(o "ET")++(o "LS")++(o "LR")++q++i
         ++(o "LS")++(o "LT")++q1

testTsDE = ptest' "Test loading functional do while statement with extra"
  ((p,BStatement),r) (loadFS>>loadFS) (p2++p)
  where r=Free (DW (Pure (q', BNmspId $ Left []))
                   (Pure (q2', BString [F,F,T,T])))
        q=(o "AN")++(o "EN")
        q'=q++[{-Terminate-}]
        q2=(o "CS")++[F,F,T,T]++(o "ES")
        q2'=q2++[{-Terminate-}]
        p=(o "TS")++(o "DW")++(o "LS")++(o "LN")++q++(o "LS")++(o "LT")++q2
        p2= (o "TS")++(o "DW")++(o "LS")++(o "LI")++[T]++(o "ES")++(o "LS")
          ++(o "LN")++(o "AN")++(o "EN")

testTsM = ptest' "Test loading functional 2-var mathematical statement"
  ((p,BStatement),r) loadFS p
  where r=Free (MSB "OD" (Pure (q', BInteger 7)) 
                         (Pure (q2', BRational 5 9)))
        q=[F]++(o "CS")++[F,T,T,T]++(o "ES")
        q'=q++[{-Terminate-}]
        q2= [F]++(o "CS")++[F,T,F,T]++(o "ES")
          ++[F]++(o "CS")++[T,F,F,T]++(o "ES")
        q2'=q2++[{-Terminate-}]
        p=(o "MS")++(o "OD")++(o "LS")++(o "LI")++q++(o "LS")++(o "LR")++q2

testTsME = ptest' "Test loading functional 1-var math statement with extra"
  ((p,BStatement),r) (loadFS>>loadFS) (p2++p)
  where r=Free (MSA "TN" (Pure (q', BString [T,F,T,F])))
        q=(o "CS")++[T,F,T,F]++(o "ES")
        q'=q++[{-Terminate-}]
        p=(o "MS")++(o "TN")++(o "LS")++(o "LT")++q
        p2= (o "MS")++(o "BN")++(o "LS")++(o "LT")++(o "ES")

-- I/O Statement Tests
testIO = ptest' "Test loading i/o statement"
  ((p,BStatement),r) loadIO p
  where r=Free (IOS (Pure (q', BInteger (-4))))
        q=[T]++(o "CS")++[T,T,F,F]++(o "ES")
        q'=q++[{-Terminate-}]
        p=(o "LS")++(o "LI")++q

testIOE = ptest' "Test loading i/o statement with extra"
  ((p,BStatement),r) (loadIO>>loadIO) (p2++p)
  where r=Free (IOS (Pure (q', BString s)))
        s=[T,T,F,F]
        q=(o "CS")++s++(o "ES")
        q'=q++[{-Terminate-}]
        p=(o "LS")++(o "LT")++q
        p2=(o "LS")++(o "LT")++(o "CS")++[F,F,T,F]++(o "ES")

-- Load Statement Tests
testLtS = ptest' "Test loading embedded literal string"
  ((p,BStatement),Pure (p1', BString s)) loadStmt p
  where s=[T,T,F,F]
        p1=(o "CS")++s++(o "ES")
        p1'=p1++[{-Terminate-}]
        p=(o "LS")++(o "LT")++p1

testLtRE = ptest' "Test loading embedded literal rational with extra"
  ((p,BStatement),Pure (p1', BRational 0 0)) (loadStmt>>loadStmt) (p2++p)
  where p1=[T]++(o "CS")++[F,F,T,T]++(o "ES")++[F]++(o "ES")
        p1'=p1++[{-Terminate-}]
        p=(o "LS")++(o "LR")++p1
        p2=(o "LS")++(o "LR")++[F]++(o "ES")++[T]++(o "CS")++[T,T,T,T]++(o "ES")

testLtI = ptest' "Test loading embedded i/o statement"
  ((p,BStatement),r) loadStmt p
  where r=Free (IOS (Pure (q', BRational 7 (-9))))
        q=[F]++(o "CS")++[F,T,T,T]++(o "ES")++[T]++(o "CS")++[F,T,T,T]++(o "ES")
        q'=q++[{-Terminate-}]
        p=(o "IO")++(o "LS")++(o "LR")++q

testLtIE = ptest' "Test loading embedded i/o statement with extra"
  ((p,BStatement),r) (loadStmt>>loadStmt) (p2++p)
  where r=Free (IOS (Pure (q', BNmspId $ Right [Parent,Child [F,T,F,T]])))
        q=(o "RN")++(o "PN")++(o "CN")++(o "CS")++[F,T,F,T]++(o "ES")++(o "ERN")
        q'=q++[{-Terminate-}]
        p=(o "IO")++(o "LS")++(o "LN")++q
        p2=(o "IO")++(o "LS")++(o "LI")++[F]++(o "ES")

testLtF = ptest' "Test loading embedded execute statement - 1 parameters"
  ((p,BStatement),r) loadStmt p
  where r=Free (ET (Pure (q', BRational 1 (-1))) [
           Pure (q1',BString [T,T,F,T])
           ])
        q=[F]++(o "CS")++[F,F,F,T]++(o "ES")++[T]++(o "CS")++[T,T,T,T]++(o "ES")
        q'=q++[{-Terminate-}]
        i=[T]++(o "CS")++[T,T,T,T]++(o "ES")
        q1=(o "CS")++[T,T,F,T]++(o "ES")
        q1'=q1++[{-Terminate-}]
        p= (o "FS")++(o "TS")++(o "ET")++(o "LS")++(o "LR")++q++i
         ++(o "LS")++(o "LT")++q1

testLtFE = ptest' "Test loading embedded 1-var math statement with extra"
  ((p,BStatement),r) (loadStmt>>loadStmt) (p2++p)
  where r=Free (MSA "TN" (Pure (q', BString [T,F,T,F])))
        q=(o "CS")++[T,F,T,F]++(o "ES")
        q'=q++[{-Terminate-}]
        p=(o "FS")++(o "MS")++(o "TN")++(o "LS")++(o "LT")++q
        p2= (o "FS")++(o "MS")++(o "OU")++(o "LS")++(o "LR")++[F]++(o "CS")
          ++[F,F,T,F]++(o "ES")++[F]++(o "CS")++[F,F,T,T]++(o "ES")++(o "LS")
          ++(o "LI")++[F]++(o "CS")++[T,F,F,T]++(o "ES")

mainList = TestLabel "Statements" $
  TestList [ testLsS, testLsSE, testLsI, testLsR, testLsN, testLsM, testLsME
           , testCtA, testCtAE, testCtR, testCtRE, testCtS, testCtSE, testCtI
           , testCtIE, testCtD, testCtDE, testCtE0, testCtE1, testCtE4
           , testCtE1, testMA, testMAE, testMB, testMBE, testTsE1, testTsDE
           , testTsM, testTsME, testIO, testIOE, testLtS, testLtRE , testLtI
           , testLtIE, testLtF, testLtFE
           ]

main = runTestTT $
  TestList [ B.mainList, O.mainList, D.mainList, mainList ]
