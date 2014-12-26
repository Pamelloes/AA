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
-- This module defines the DataTypes and provides associated functionality for
-- Strings, Integers, Rationals, and Namespaces in accordance with Sections IV
-- and V of the Advanced Assembly 0.5.0 language specification.
module DataType where

import BitSeries
import qualified Data.Map as M
import Opcodes

data RNmspS = Child BitSeries | Parent deriving Show
type RNmsp = [RNmspS]
type ANmsp = [BitSeries]
data Primitive = BString BitSeries | BInteger Integer | BRational Integer Integer
               | BNmspId (Either ANmsp RNmsp) | BStatement deriving Show
type DataType = (BitSeries,Primitive) 

type Namespaces = M.Map ANmsp DataType

-- Strings
pstring :: BitSeries -> (BitSeries,Primitive)
pstring [] = error "lstring: Reached program end"
pstring s
  | snd es = (fst es,BString [])
  | snd cs = let (a,BString b)=pstring prog in (a,BString (str++b))
  where es=hasOpcode s "ES"
        cs=hasOpcode s "CS"
        (str,prog) = splitAt 4 (fst cs)

lstring = snd . pstring

cstring :: DataType -> DataType
cstring a@(_,BString _) = a
cstring (b,_) = (b,lstring b)

-- Integers
bsToInt :: [Bit] -> Integer
bsToInt [] = 0
bsToInt x = val + 16*(bsToInt remainder)
  where (base,remainder) = splitAt 4 x
        val = foldl (\x y->2*x+(if y==T then 1 else 0)) 0 base

pinteger :: BitSeries -> (BitSeries,Primitive)
pinteger [] = error "linteger: Reached program end"
pinteger (sign:remainder) = (prog,BInteger (sgn $ bsToInt str))
  where (prog,BString str)=pstring remainder
        sgn=if sign==T then (\x->x-2^(length str)) else (\x->x)

linteger = snd . pinteger

cinteger :: DataType -> DataType
cinteger a@(_,BInteger _) = a
cinteger (b,_) = (b,linteger b)

-- Rationals
prational :: BitSeries -> (BitSeries,Primitive)
prational [] = error "lrational: Reached program end"
prational s = (prog,BRational i1 i2)
  where (p1,BInteger i1a)=pinteger s
        (prog,BInteger i2)=pinteger p1
        i1=if i2 == 0 then 0 else i1a

lrational = snd . prational

crational :: DataType -> DataType
crational a@(_,BRational _ _) = a
crational (b,_) = (b,lrational b)

-- Namespaces
panmsp :: BitSeries -> (BitSeries,ANmsp)
panmsp p
  | snd en = (fst en,[])
  | snd cn = let (pr,id)=panmsp prog in (pr,str:id)
  where en=hasOpcode p "EN"
        cn=hasOpcode p "CN"
        (prog, BString str)=pstring (fst cn)

prnmsp :: BitSeries -> (BitSeries,RNmsp)
prnmsp p
  | snd en = (fst en,[])
  | snd cn = let (pr,id)=prnmsp prog in (pr,(Child str):id)
  | snd pn = let (pr,id)=prnmsp (fst pn) in (pr,Parent:id)
  where en=hasOpcode p "ERN"
        cn=hasOpcode p "CN"
        (prog, BString str)=pstring (fst cn)
        pn=hasOpcode p "PN"

pnmsp :: BitSeries -> (BitSeries, Primitive)
pnmsp p
  | snd abs = let (a,b)=panmsp (fst abs) in (a,BNmspId $ Left b)
  | snd rel = let (a,b)=prnmsp (fst rel) in (a,BNmspId $ Right b)
  where abs=hasOpcode p "AN"
        rel=hasOpcode p "RN"

lnmsp = snd . pnmsp

cnmsp :: DataType -> DataType
cnmsp a@(_,BNmspId _) = a
cnmsp (b,_) = (b,lnmsp b)

rnmsp :: ANmsp -> RNmsp
rnmsp = fmap (\x -> Child x)

anmsp :: ANmsp -> RNmsp -> ANmsp
anmsp a [] = a 
anmsp a ((Child s):cs) = anmsp (a++[s]) cs
anmsp [] (Parent:cs) = anmsp [] cs
anmsp a (Parent:cs) = anmsp (init a) cs

gnmsp :: ANmsp -> DataType -> ANmsp
gnmsp _ (_,BNmspId (Left i)) = i
gnmsp b (s,BNmspId (Right r)) = gnmsp b (s,BNmspId (Left (anmsp b r)))
gnmsp b s = gnmsp b (cnmsp s)

defaultNamespace :: BitSeries -> Namespaces
defaultNamespace p = M.fromList [([],(p,BStatement))]

nmspValue :: ANmsp -> DataType -> Namespaces -> DataType
nmspValue a d n = if M.member i n then n M.! i else (repeat Terminate,BString [])
  where i=gnmsp a d

nmspValueSet :: ANmsp -> DataType -> DataType -> Namespaces -> Namespaces
nmspValueSet a d = M.insert i
  where i = gnmsp a d
  
-- Statements
cstmt :: DataType -> DataType
cstmt a@(_,BStatement)=a
cstmt (s,_)=(s,BStatement)
