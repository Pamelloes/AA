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
-- This module defines a Bit and BitSeries. A special Bit implementation is used
-- to accomodate lists that end with a repeating false value.
{-# LANGUAGE PatternSynonyms #-}
module Language.AA.BitSeries where

import Text.Parsec.Pos
import Text.Parsec.Prim

-- Synonym definitions.
type Bit = Bool
pattern T = True
pattern F = False

type BitSeries=[Bit]

-- Parsec parsers
btoken :: (Monad m) => Bit -> ParsecT BitSeries u m Bit
btoken x = tokenPrim (show) (\c x xs -> newPos "BitSeries" (-1) (-1)) 
                 (\y -> if x==y then Just y else Nothing)

btokens :: (Monad m) => BitSeries -> ParsecT BitSeries u m BitSeries
btokens x = mapM btoken x
