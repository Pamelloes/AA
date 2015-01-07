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
-- and V of the Advanced Assembly 0.5.1 language specification.
module DataType where

import BitSeries
import qualified Data.Map as M
import Opcodes

-- Namespace Types
data RNmspS = Child BitSeries | Parent deriving Show
type RNmsp = [RNmspS]
type ANmsp = [BitSeries]
-- Global Types
data Primitive = BString BitSeries | BInteger Integer | BRational Integer Integer
               | BNmspId (Either ANmsp RNmsp) | BStatement deriving Show
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

lstring = snd . snd . pstring

cstring :: DataType -> DataType
cstring a@(_,BString _) = a
cstring (b,_) = (b,lstring b)

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

linteger = snd . snd . pinteger

cinteger :: DataType -> DataType
cinteger a@(_,BInteger _) = a
cinteger (b,_) = (b,linteger b)

-- Rationals
prational :: BitSeries -> (BitSeries,DataType)
prational [] = error "lrational: Reached program end"
prational s = (prog,(p++p2,BRational i1 i2))
  where (p1,(p,BInteger i1a))=pinteger s
        (prog,(p2,BInteger i2))=pinteger p1
        i1=if i2 == 0 then 0 else i1a

lrational = snd . snd . prational

crational :: DataType -> DataType
crational a@(_,BRational _ _) = a
crational (b,_) = (b,lrational b)

-- Namespaces
panmsp :: BitSeries -> (BitSeries,(BitSeries,ANmsp))
panmsp p
  | snd en = (fst en,(opcodes M.! "EN",[]))
  | snd cn = let (pr,(q,id))=panmsp prog in (pr,(cno++pstr++q,str:id))
  where en=hasOpcode p "EN"
        cn=hasOpcode p "CN"
        cno=opcodes M.! "CN"
        (prog, (pstr,BString str))=pstring (fst cn)

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

pnmsp :: BitSeries -> (BitSeries, DataType)
pnmsp p
  | snd abs = let (a,(q,b))=panmsp (fst abs) in (a,(an++q,BNmspId $ Left b))
  | snd rel = let (a,(q,b))=prnmsp (fst rel) in (a,(rn++q,BNmspId $ Right b))
  where abs=hasOpcode p "AN"
        an=opcodes M.! "AN"
        rel=hasOpcode p "RN"
        rn=opcodes M.! "RN"

lnmsp = snd . snd . pnmsp

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
