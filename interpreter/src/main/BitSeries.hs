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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
module BitSeries where

import Data.Bits
import Data.Data
import Data.Typeable
import Text.Parsec.Pos
import Text.Parsec.Prim

-- A Bit can be either T (True) or F (False). Terminate is used in infinite
-- lists to indicate the end of non-repeating data.
data Bit = T | F | Terminate deriving (Show,Data,Typeable)
instance Eq Bit where
  T /= F = True
  F /= T = True
  T /= Terminate = True
  Terminate /= T = True
  _ /= _ = False
instance Ord Bit where
  T `compare` a = if a == F then GT else EQ
  a `compare` T = if a == F then LT else EQ
  _ `compare` _ = EQ

type BitSeries=[Bit]
instance Bits BitSeries where
  []     .&. _      = []
  _      .&. []     = [] 
  (T:as) .&. (T:bs) = T:(as.&.bs)
  (_:as) .&. (_:bs) = F:(as.&.bs)

  []     .|. _      = []
  _      .|. []     = [] 
  (T:as) .|. (_:bs) = T:(as.&.bs)
  (_:as) .|. (T:bs) = T:(as.&.bs)
  (_:as) .|. (_:bs) = F:(as.&.bs)

  []     `xor` _      = []
  _      `xor` []     = [] 
  (T:as) `xor` (T:bs) = F:(as.&.bs)
  (a:as) `xor` (b:bs) = c:(as.&.bs)
    where c=if (a==F)&&(b==F) then F else T

  complement (T        :as) = F:(complement as)
  complement (F        :as) = T:(complement as)
  complement (Terminate:as) = T:(complement as)

  shift b 0 = b
  shift b i
    | i>0 = b++(replicate i F)
    | i<0 && (abs i)<(length b) = take (i+length b) b
    | otherwise = []

  rotate [] _ = []
  rotate b  0 = b
  rotate b  i
    | i>0 = rotate (tail b++[head b]) (i-1)
    | i<0 = rotate (init b++[last b]) (i+1)

  bitSize = length
  bitSizeMaybe = return . bitSize

  isSigned _ = False

  testBit = ((== T).).(!!)

  bit i = replicate i F++[T]

  popCount = foldr (\a i -> if a==T then i+1 else i) 0
  
-- Parsec parsers
btoken :: Bit -> Parsec BitSeries u Bit
btoken x = token (show) (const $ newPos "BitSeries" (-1) (-1)) 
                 (\y -> if x==y then Just y else Nothing)

btokens :: BitSeries -> Parsec BitSeries u BitSeries
btokens x = mapM btoken x
