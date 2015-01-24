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
-- This module defines the data types and provides functions for loading
-- Strings, Integers, Rationals, and Namespaces in accordance with Sections IV
-- and V of the Advanced Assembly 0.5.2 language specification. The Statement
-- module contains functions for loading Statements. The DataType.Util module
-- contains additional functions which may be useful when processing DataTypes.
{-# LANGUAGE DeriveDataTypeable #-}
module Language.AA.DataType where

import Control.Arrow
import Control.Monad
import qualified Data.Data as D
import qualified Data.Map as M
import Data.Typeable
import Language.AA.BitSeries
import Language.AA.Opcodes
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

-- Strings
pstring :: Parsec BitSeries u DataType
pstring = do 
  let cs = do {
    op <- mopc "CS";
    cnt <- replicateM 4 anyToken;
    return (op++cnt,cnt);
  }
  pts <- many cs
  end <- mopc "ES"
  let bs = foldr ((++).fst) [] pts
  let st = foldr ((++).snd) [] pts
  return (bs++end,BString st)

-- Integers
bsToInt :: [Bit] -> Integer
bsToInt [] = 0
bsToInt x = val + 16*(bsToInt remainder)
  where (base,remainder) = splitAt 4 x
        val = foldl (\x y->2*x+(if y==T then 1 else 0)) 0 base

pinteger :: Parsec BitSeries u DataType
pinteger = do
  sign <- anyToken
  (bs,BString str) <- pstring
  let sgn = if sign==T then (\x -> x-2^(length str)) else (id)
  return (sign:bs,BInteger $ sgn $ bsToInt str)

-- Rationals
prational :: Parsec BitSeries u DataType
prational = do
  (bs,BInteger a) <- pinteger
  (bt,BInteger b) <- pinteger
  let a' = if b == 0 then 0 else a
  return $ (bs++bt,BRational a' b)

-- Namespaces
panmsp :: Parsec BitSeries u (BitSeries,ANmsp)
panmsp = do
  let cn = do {
    cn <- mopc "CN";
    (bs,BString str) <- pstring;
    return $ (cn++bs,str);
  }
  pts <- many cn
  let bs = foldr ((++).fst) [] pts
  let str = foldr ((:).snd) [] pts
  end <- mopc "EN"
  return (bs++end,str)

prnmsp :: Parsec BitSeries u (BitSeries,RNmsp)
prnmsp = do
  let cn = do {
    cn <- try $ mopc "CN";
    (bs,BString str) <- pstring;
    return (cn++bs,Child str);
  }
  let pn = do {
    pn <- try $ mopc "PN";
    return (pn,Parent);
  }
  pts <- many (pn <|> cn)
  end <- mopc "ERN"
  let bs = foldr ((++).fst) [] pts
  let pt = foldr ((:).snd) [] pts
  return (bs++end,pt)

pnmsp :: Parsec BitSeries u DataType
pnmsp = do
  let an = do {
    an <- try $ mopc "AN";
    (bs,ns) <- panmsp;
    return (an++bs,BNmspId $ Left ns);
  }
  let rn = do {
    rn <- try $ mopc "RN";
    (bs,ns) <- try $ prnmsp;
    return (rn++bs,BNmspId $ Right ns);
  }
  an <|> rn
