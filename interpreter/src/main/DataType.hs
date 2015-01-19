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
-- and V of the Advanced Assembly 0.5.1 language specification. The Statement
-- module contains functions for loading Statements. The DataType.Util module
-- contains additional functions which may be useful when processing DataTypes.
{-# LANGUAGE DeriveDataTypeable #-}
module DataType where

import BitSeries
import Control.Arrow
import Control.Monad
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
data Primitive = BString BitSeries | BInteger Integer | BRational Integer Integer
               | BNmspId (Either ANmsp RNmsp) | BStatement 
               deriving (Show,D.Data,Typeable)
type DataType = (BitSeries,Primitive) 

-- Strings
pstring :: BitSeries -> (BitSeries,DataType)
pstring [] = error "lstring: Reached program end"
pstring s
  | snd es = (fst es,(opcodes M.! "ES",BString []))
  | snd cs = let (a,(p,BString b))=pstring prog in (a,((opcodes M.! "CS")++str++p,BString (str++b)))
  where es=hasOpcode s "ES"
        cs=hasOpcode s "CS"
        (str,prog) = splitAt 4 (fst cs)

qstring :: Parsec BitSeries u DataType
qstring = do 
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

pinteger :: BitSeries -> (BitSeries,DataType)
pinteger [] = error "linteger: Reached program end"
pinteger (sign:remainder) = (prog,(sign:p,BInteger (sgn $ bsToInt str)))
  where (prog,(p,BString str))=pstring remainder
        sgn=if sign==T then (\x->x-2^(length str)) else (\x->x)

qinteger :: Parsec BitSeries u DataType
qinteger = do
  sign <- anyToken
  (bs,BString str) <- qstring
  let sgn = if sign==T then (\x -> x-2^(length str)) else (id)
  return (sign:bs,BInteger $ sgn $ bsToInt str)

-- Rationals
prational :: BitSeries -> (BitSeries,DataType)
prational [] = error "lrational: Reached program end"
prational s = (prog,(p++p2,BRational i1 i2))
  where (p1,(p,BInteger i1a))=pinteger s
        (prog,(p2,BInteger i2))=pinteger p1
        i1=if i2 == 0 then 0 else i1a

qrational :: Parsec BitSeries u DataType
qrational = do
  (bs,BInteger a) <- qinteger
  (bt,BInteger b) <- qinteger
  let a' = if b == 0 then 0 else a
  return $ (bs++bt,BRational a' b)

-- Namespaces
panmsp :: BitSeries -> (BitSeries,(BitSeries,ANmsp))
panmsp p
  | snd en = (fst en,(opcodes M.! "EN",[]))
  | snd cn = let (pr,(q,id))=panmsp prog in (pr,(cno++pstr++q,str:id))
  where en=hasOpcode p "EN"
        cn=hasOpcode p "CN"
        cno=opcodes M.! "CN"
        (prog, (pstr,BString str))=pstring (fst cn)

qanmsp :: Parsec BitSeries u (BitSeries,ANmsp)
qanmsp = do
  let cn = do {
    cn <- mopc "CN";
    (bs,BString str) <- qstring;
    return $ (cn++bs,str);
  }
  pts <- many cn
  let bs = foldr ((++).fst) [] pts
  let str = foldr ((:).snd) [] pts
  end <- mopc "EN"
  return (bs++end,str)

prnmsp :: BitSeries -> (BitSeries,(BitSeries,RNmsp))
prnmsp p
  | snd en = (fst en,(opcodes M.! "ERN",[]))
  | snd cn = let (pr,(q,id))=prnmsp prog in (pr,(cno++pstr++q,(Child str):id))
  | snd pn = let (pr,(q,id))=prnmsp (fst pn) in (pr,(pno++q,Parent:id))
  where en=hasOpcode p "ERN"
        cn=hasOpcode p "CN"
        (prog, (pstr,BString str))=pstring (fst cn)
        cno=opcodes M.! "CN"
        pn=hasOpcode p "PN"
        pno=opcodes M.! "PN"

qrnmsp :: Parsec BitSeries u (BitSeries,RNmsp)
qrnmsp = do
  let cn = do {
    cn <- try $ mopc "CN";
    (bs,BString str) <- qstring;
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

pnmsp :: BitSeries -> (BitSeries, DataType)
pnmsp p
  | snd abs = let (a,(q,b))=panmsp (fst abs) in (a,(an++q,BNmspId $ Left b))
  | snd rel = let (a,(q,b))=prnmsp (fst rel) in (a,(rn++q,BNmspId $ Right b))
  where abs=hasOpcode p "AN"
        an=opcodes M.! "AN"
        rel=hasOpcode p "RN"
        rn=opcodes M.! "RN"

qnmsp :: Parsec BitSeries u DataType
qnmsp = do
  let an = do {
    an <- try $ mopc "AN";
    (bs,ns) <- qanmsp;
    return (an++bs,BNmspId $ Left ns);
  }
  let rn = do {
    rn <- try $ mopc "RN";
    (bs,ns) <- try $ qrnmsp;
    return (rn++bs,BNmspId $ Right ns);
  }
  an <|> rn
