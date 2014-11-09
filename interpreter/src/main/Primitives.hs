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

import Opcodes

-- NOTE: The program is assumed to be an infinitely long list (see language
-- specification). If a non-infinite list is provided, behavior is undefined.

type BString = [Bool]
type BInt = Integer

-- Result Program has BString removed.
lstring :: Program -> (Program,BString)
lstring [] = error "lstring: Reached program end"
lstring s
  | snd es = (fst es,[])
  | snd cs = let (a,b)=lstring prog in (a,str++b)
  where es=hasOpcode s "ES"
        cs=hasOpcode s "CS"
        (str,prog) = splitAt 4 (fst cs)

bsToInt :: BString -> Integer
bsToInt [] = 0
bsToInt x = val + 16*(bsToInt remainder)
  where (base,remainder) = splitAt 4 x
        val = foldl (\x y->2*x+(if y then 1 else 0)) 0 base

-- Result Program has BInt removed.
linteger :: Program -> (Program,BInt)
linteger [] = error "linteger: Reached program end"
linteger (sign:remainder) = (prog,sgn $ bsToInt str)
  where (prog,str)=lstring remainder
        sgn=if sign then (\x->x-2^(length str)) else (\x->x)
