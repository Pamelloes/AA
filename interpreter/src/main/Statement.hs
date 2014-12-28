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

data Free f n = Free (f (Free f n)) | Pure n 
instance (Functor f, Show a) => Show (Free f a) where
  showsPrec d (Pure a) = showParen (d > 10) $
    showString "Pure " . showsPrec 11 a
  showsPrec d (Free m) = showParen (d > 10) $
    showString "Free " . showsPrec 11 m

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
type DStmt a = (DataType,Free Stmt ())

dpre :: BitSeries -> (BitSeries,DStmt a) -> (BitSeries,DStmt a)
dpre a (b,((c,BStatement i),t)) = (b,((a++c,BStatement i),t))

loadLS :: Integer -> BitSeries -> (BitSeries,DStmt n)
loadLS i s 
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
                dt=(p,BStatement i)
                ds=(dt,Free (LS d) $ Pure ())

loadTS :: Integer -> BitSeries -> (BitSeries, DStmt a)
loadTS = undefined

loadMS :: Integer -> BitSeries -> (BitSeries, DStmt a)
loadMS = undefined

loadFS :: Integer -> BitSeries -> (BitSeries, DStmt a)
loadFS i s
  | snd ts = dpre tso $ loadTS i s
  | snd ms = dpre mso $ loadMS i s
  where ts = hasOpcode s "TS"
        tso = opcodes M.! "TS"
        ms = hasOpcode s "MS"
        mso = opcodes M.! "MS"

loadIO :: Integer -> BitSeries -> (BitSeries, DStmt a)
loadIO = undefined

loadStmt :: Integer -> BitSeries -> (BitSeries, DStmt a)
loadStmt i s
  | snd ls = dpre lso $ loadLS i s
  | snd fs = dpre fso $ loadFS i s
  | snd io = dpre ioo $ loadIO i s
  where ls = hasOpcode s "LS"
        lso = opcodes M.! "LS"
        fs = hasOpcode s "FS"
        fso = opcodes M.! "FS"
        io = hasOpcode s "IO"
        ioo = opcodes M.! "IO"

loadEStmt :: BitSeries -> (BitSeries, DStmt a)
loadEStmt p = dpre q $ loadStmt n r
  where (r,(q,BInteger n))=pinteger p
