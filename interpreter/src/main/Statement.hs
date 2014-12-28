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
-- This module generates a program's AST in accordance with Section VI of the
-- Advanced Assembly 0.5.0 specification.
module Statement where

import BitSeries
import qualified Data.Map as M
import DataType
import Opcodes

data Free f n = Free f (Free f n) | Pure n deriving Show
{-
instance (Functor f) => Monad (Free f) where
  return = Pure
  (Free x) >>= f = Free (fmap (>>= f) x)
  (Pure r) >>= f = f r
-}

data Stmt next = LS DataType
              -- Control Statements
               | AS next next
               | RS next
               | ET [next]
               | SQ next next
               | IF next next next
               | DW next next
              -- Mathematical Statements 
               | MSA Opcode next
               | MSB Opcode next next
              -- IO Statements
               | IOS next
               deriving Show
type DStmt a = (DataType,Free (Stmt a) ())

loadLS :: BitSeries -> (BitSeries,DStmt n)
loadLS s 
  | snd lt = pr "LT" $ pstring (fst lt)
  | snd li = pr "LI" $ pinteger (fst li)
  | snd lr = pr "LR" $ prational (fst lr)
  | snd ln = pr "LN" $ pnmsp (fst ln)
  | snd lm = pr "LM" $ let (b,(d,_))=loadEStmt (fst lm) in (b,d)
  where lt = hasOpcode s "LT"
        li = hasOpcode s "LI"
        lr = hasOpcode s "LR"
        ln = hasOpcode s "LN"
        lm = hasOpcode s "LM"
        pr :: Opcode -> (BitSeries,DataType) -> (BitSeries,DStmt n)
        pr o (t,d) = (t,ds)
          where p=(opcodes M.! o)++(fst d)
                dt=(p,BStatement)
                ds=(dt,Free (LS d) $ Pure ())

loadTS :: BitSeries -> (BitSeries, DStmt a)
loadTS = undefined

loadMS :: BitSeries -> (BitSeries, DStmt a)
loadMS = undefined

loadFS :: BitSeries -> (BitSeries, DStmt a)
loadFS s
  | snd ts = let (b,((c,BStatement),t))=loadTS s in (b,((tso++c,BStatement),t))
  | snd ms = let (b,((c,BStatement),t))=loadMS s in (b,((mso++c,BStatement),t))
  where ts = hasOpcode s "TS"
        tso = opcodes M.! "TS"
        ms = hasOpcode s "MS"
        mso = opcodes M.! "MS"

loadIO :: BitSeries -> (BitSeries, DStmt a)
loadIO = undefined

loadStmt :: BitSeries -> (BitSeries, DStmt a)
loadStmt s
  | snd ls = let (b,((c,BStatement),t))=loadLS s in (b,((lso++c,BStatement),t))
  | snd fs = let (b,((c,BStatement),t))=loadFS s in (b,((fso++c,BStatement),t))
  | snd io = let (b,((c,BStatement),t))=loadIO s in (b,((ioo++c,BStatement),t))
  where ls = hasOpcode s "LS"
        lso = opcodes M.! "LS"
        fs = hasOpcode s "FS"
        fso = opcodes M.! "FS"
        io = hasOpcode s "IO"
        ioo = opcodes M.! "IO"

loadEStmt :: BitSeries -> (BitSeries, DStmt a)
loadEStmt = undefined
