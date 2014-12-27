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

loadLS :: BitSeries -> (BitSeries,Free (Stmt n) ())
loadLS s 
  | snd lt = undefined
  | snd li = undefined
  | snd lr = undefined
  | snd ln = undefined
  | snd lm = undefined
  where lt = hasOpcode s "LT"
        li = hasOpcode s "LI"
        lr = hasOpcode s "LR"
        ln = hasOpcode s "LN"
        lm = hasOpcode s "LM"
--        pr :: BitSeries -> (BitSeries,Primitive) -> (BitSeries,Free (Stmt n) ())
--        pr s (t,p) = (t,Free (LS (

loadCS :: BitSeries -> (BitSeries,Free (Stmt n) r)
loadCS = undefined

loadIO :: BitSeries -> (BitSeries,Free (Stmt n) r)
loadIO = undefined

loadStmt :: BitSeries -> (BitSeries, Free (Stmt n) r)
loadStmt = undefined

loadEStmt :: BitSeries -> (BitSeries, Free (Stmt n) r)
loadEStmt = undefined
