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
-- This module contains functions for parsing data types in Advanced Assembly
module Primitives where

import Data.Ratio
import Opcodes

-- 

data Primitive = BString [Bit] | BInt Integer [Bit]
               | BRtnl Integer Integer [Bit] deriving (Show, Eq)
instance Ord Primitive where
  BString a `compare` BInt _ b = a `compare` b
  BInt _ a `compare` BString b = a `compare` b
  BString a `compare` BRtnl _ _ b = a `compare` b
  BRtnl _ _ a `compare` BString b = a `compare` b
  BInt _ a `compare` BRtnl _ _ b = a `compare` b
  BRtnl _ _ a `compare` BInt _ b = a `compare` b
  BString a `compare` BString b = a `compare` b
  BInt a _ `compare` BInt b _ = a `compare` b
  BRtnl a b _ `compare` BRtnl c d _ = (a%b) `compare` (c%d)

-- NOTE: The program is assumed to be an infinitely long list (see language
-- specification). If a non-infinite list is provided, behavior is undefined.

-- Result Program has BString removed.
lstring :: Program -> (Program,Primitive)
lstring [] = error "lstring: Reached program end"
lstring s
  | snd es = (fst es,BString [])
  | snd cs = let (a,BString b)=lstring prog in (a,BString (str++b))
  where es=hasOpcode s "ES"
        cs=hasOpcode s "CS"
        (str,prog) = splitAt 4 (fst cs)

bsToInt :: [Bit] -> Integer
bsToInt [] = 0
bsToInt x = val + 16*(bsToInt remainder)
  where (base,remainder) = splitAt 4 x
        val = foldl (\x y->2*x+(if y==T then 1 else 0)) 0 base

-- Result Program has BInt removed.
linteger :: Program -> (Program,Primitive)
linteger [] = error "linteger: Reached program end"
linteger (sign:remainder) = (prog,BInt (sgn $ bsToInt str) (sign:str))
  where (prog,BString str)=lstring remainder
        sgn=if sign==T then (\x->x-2^(length str)) else (\x->x)
