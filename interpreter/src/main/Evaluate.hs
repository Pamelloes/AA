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
-- This module evaluates a program's AST in accordance with Sections VI and VII
-- of the Advanced Assembly 0.5.1 specification.
module Evaluate where

import BitSeries
import Control.Arrow
import qualified Data.Map as M
import DataType
import Opcodes
import Statement

type State = (ANmsp,Namespaces)

-- Namespace definitions
type Namespaces = M.Map ANmsp DataType

defaultNamespace :: BitSeries -> Namespaces
defaultNamespace p = M.fromList [([],(p,BStatement))]

nmspValue :: ANmsp -> DataType -> Namespaces -> DataType
nmspValue a b =  M.findWithDefault (repeat Terminate,BString []) (gnmsp a b)

nmspValue' :: State -> DataType -> (State,DataType)
nmspValue' a b = (a,nmspValue (fst a) b (snd a))

nmspValueSet :: ANmsp -> DataType -> DataType -> Namespaces -> Namespaces
nmspValueSet a b = M.insert $ gnmsp a b

nmspValueSet' :: State -> DataType -> DataType -> (State,DataType)
nmspValueSet' a b c = (s a,c)
  where s = second . const $ nmspValueSet (fst a) b c (snd a)

-- Utilities
{-
estring   a = first cstring   . (evaluate a)
einteger  a = first cinteger  . (evaluate a)
erational a = first crational . (evaluate a)
enmsp     a = first cnmsp     . (evaluate a)
-}

-- Evaluate definitions
evaluate :: Free Stmt () -> State -> (State,DataType)
evaluate (Free (LS a))   s = (s,a)
evaluate (Free (AS a b)) s = nmspValueSet' s3 av bv
  where (s2,av) = evaluate a s
        (s3,bv) = evaluate b s2
evaluate (Free (RS a))   s = nmspValue' s2 av
  where (s2,av) = evaluate a s
{-
evaluate (Free (RS a)) = "\nRetrieve: "++showProgram a
showProgram (Free (ET a b)) = "\nExecute:\nNmsp: "++showProgram a++snd c
  where c = foldl (\(i,s) a->(i+1,s++"\n("++show (i+1)++") "++showProgram a))
                  (0,"") b
showProgram (Free (SQ a b)) = "\nSequence:\n(1) "++showProgram a++"\b(2) "
  ++showProgram b
showProgram (Free (IF a b c)) = "\nIf: "++showProgram a++"\nThen: "++showProgram b
  ++"\nElse: "++showProgram c
showProgram (Free (DW a b)) = "\nDo: "++showProgram a++"\nWhile: "
  ++showProgram b
showProgram (Free (MSA a b)) = "\nOperation "++a++": "++showProgram b
showProgram (Free (MSB a b c)) = "\nOperation "++a++":\n(1) "++showProgram b
  ++"\n(2) "++showProgram c
showProgram (Free (IOS a)) = "\nI/O: "++showProgram a
-}
