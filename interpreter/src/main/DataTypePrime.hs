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
-- This module consists of an alternate implementation of the various primitives
-- in the hope of creating a more elegant parsing system.
{-# LANGUAGE DeriveDataTypeable #-}
module DataTypePrime where

import BitSeries
import Control.Arrow
import Control.Monad
import Control.Monad.Writer.Lazy
import qualified Data.Data as D
import qualified Data.Map as M
import Data.Typeable
import Opcodes
import Text.Parsec.Combinator
import Text.Parsec.Prim

-- Namespace Types
data RNmspS = Child BitSeries | Parent deriving (Show,D.Data,Typeable)
type RNmsp = [RNmspS]
type ANmsp = [BitSeries]
-- Global Types
data Primitive = BString BitSeries | BInteger Integer 
               | BRational Integer Integer | BNmspId (Either ANmsp RNmsp) 
               | BStatement deriving (Show,D.Data,Typeable)
type DataType = (BitSeries,Primitive) 
type DataType' = Writer BitSeries Primitive

-- Strings
pstring :: Parsec BitSeries u DataType
pstring = undefined

-- Integers
bsToInt :: [Bit] -> Integer
bsToInt [] = 0
bsToInt x = val + 16*(bsToInt remainder)
  where (base,remainder) = splitAt 4 x
        val = foldl (\x y->2*x+(if y==T then 1 else 0)) 0 base

pinteger :: Parsec BitSeries u DataType
pinteger = undefined

-- Rationals
prational :: Parsec BitSeries u DataType
prational = undefined 

-- Namespaces
panmsp :: Parsec BitSeries u (BitSeries,ANmsp)
panmsp = undefined

prnmsp :: Parsec BitSeries u (BitSeries,RNmsp)
prnmsp = undefined

pnmsp :: Parsec BitSeries u DataType
pnmsp = undefined
