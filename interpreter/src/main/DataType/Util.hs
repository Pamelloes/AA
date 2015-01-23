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
-- This module provides various utility functions for dealing with DataTypes.
module DataType.Util where

import BitSeries
import Control.Arrow
import DataType
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Ratio
import Opcodes
import Text.Parsec.Prim

nmspcmp :: ANmsp -> ANmsp -> Ordering
nmspcmp []     []     = EQ
nmspcmp _      []     = LT
nmspcmp []     _      = GT
nmspcmp (a:as) (b:bs) = if c == EQ then nmspcmp as bs else c
  where c = compare a b

-- Compares two DataTypes in accordance with Section VI.B.2 of the Advanced
-- Assembly 0.5.1 Specification
cmpdt :: DataType -> DataType -> ANmsp -> Ordering
cmpdt (_,BString a)     (_,BString b)     _ = compare a b
cmpdt (_,BInteger a)    (_,BInteger b)    _ = compare a b
cmpdt (_,BRational _ 0) (_,BRational _ 0) _ = EQ
cmpdt (_,BRational _ 0) (_,BRational _ _) _ = LT
cmpdt (_,BRational _ _) (_,BRational _ 0) _ = GT
cmpdt (_,BRational a b) (_,BRational c d) _ = compare (a%b) (c%d)
cmpdt a@(_,BNmspId _)   b@(_,BNmspId _)   s = nmspcmp av bv
  where av = gnmsp s a
        bv = gnmsp s b
cmpdt (a,BStatement)    (b,BStatement)    _ = compare a b

parseDT :: Parsec BitSeries () DataType -> BitSeries -> DataType
parseDT p b = e $ runP p () "" (b++repeat F)
  where e :: (Show a) => Either a DataType -> DataType
        e (Left a) = error $ show a
        e (Right (bs, p)) = (b,p)

-- String Utilities
lstring = snd . parseDT pstring

cstring :: DataType -> DataType
cstring a@(_,BString _) = a
cstring (b,_) = (b,lstring b)

bsToString :: BitSeries -> BitSeries
bsToString a = foldr (\c a -> (o "CS")++l c++a) (o "ES") $ chunksOf 4 a
  where l s = if (length s)<4 then l (s++[F]) else s
        o t = opcodes M.! t

bsToDT :: BitSeries -> DataType
bsToDT s = (bsToString s, BString s)

-- Integer Utilities
linteger = snd . parseDT pinteger

cinteger :: DataType -> DataType
cinteger a@(_,BInteger _) = a
cinteger (b,_) = (b,linteger b)

intToBin :: Integer -> BitSeries
intToBin x = snd $ until ((==0).fst) (\(i,s)->second (s++) $ fbit i []) (x',[])
  where neg = x < 0
        fbit i s | length s == 4 = (i,s)
                 | otherwise     = fbit (i`quot`2) (b:s)
                 where b = if (i`mod`2==0)/=neg then F else T
        x' = if neg then x+1 else x

intToBS :: Integer -> BitSeries
intToBS i = [s]++bsToString (intToBin i)
  where s=if i<0 then T else F

intToDT :: Integer -> DataType
intToDT i = (intToBS i,BInteger i)

-- Rational Utilities
lrational = snd . parseDT prational

crational :: DataType -> DataType
crational a@(_,BRational _ _) = a
crational (b,_) = (b,lrational b)

rtlToBS :: Integer -> Integer -> BitSeries
rtlToBS a b = (intToBS a)++(intToBS b)
rtlToBS' :: Rational -> BitSeries
rtlToBS' a = rtlToBS (numerator a) (denominator a)

rtlToDT :: Integer -> Integer-> DataType
rtlToDT a b = (rtlToBS a b,BRational a b)
rtlToDT' :: Rational -> DataType
rtlToDT' a = rtlToDT (numerator a) (denominator a)

-- Namespace Utilities
lnmsp = snd . parseDT pnmsp

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

-- Statement Utilities
cstmt :: DataType -> DataType
cstmt = second (const BStatement)

-- Boolean Utilities
-- Convert DataType to and from Bool in accordance with Section IV of the 
-- Advanced Assembly 0.5.1 Specification
dtToBool :: DataType -> Bool
dtToBool (_,BString []) = False
dtToBool (_,BInteger 0) = False
dtToBool (_,BRational 0 x) = x == 0
dtToBool (_,BNmspId (Left [])) = False
dtToBool (x,BStatement)
  | isPrefixOf ((o "LS")++(o "LT")++(o "ES")) x = False
  | otherwise = True
  where o t = opcodes M.! t
dtToBool _ = True

boolToDT :: Primitive -> Bool -> DataType
boolToDT (BString _)     False = bsToDT []
boolToDT (BString _)     True  = bsToDT [F,F,F,F]
boolToDT (BInteger _)    False = intToDT 0
boolToDT (BInteger _)    True  = intToDT 1
boolToDT (BRational _ _) False = rtlToDT 0 1
boolToDT (BRational _ _) True  = rtlToDT (-1) (-1)
boolToDT (BNmspId _)     False = (p,BNmspId $ Left [])
  where p = (o "AN")++(o "EN")
        o t = opcodes M.! t
boolToDT (BNmspId _)     True  = (p,BNmspId $ Left [[]])
  where p = (o "AN")++(o "CN")++(o "ES")++(o "EN")
        o t = opcodes M.! t
boolToDT (BStatement)    False = (p,BStatement)
  where p = (o "LS")++(o "LT")++(o "ES")
        o t = opcodes M.! t
boolToDT (BStatement)    True  = (p,BStatement)
  where p = (o "LS")++(o "LT")++(o "CS")++[F,F,F,F]++(o "ES")
        o t = opcodes M.! t
