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

-- General Utilities
arraycmp :: Ord a => [a] -> [a] -> Ordering
arraycmp []     []     = EQ
arraycmp _      []     = GT
arraycmp []     _      = LT
arraycmp (a:as) (b:bs) = if c == EQ then arraycmp as bs else c
  where c = a `compare` b
  
nmspcmp :: ANmsp -> ANmsp -> Ordering
nmspcmp []     []     = EQ
nmspcmp _      []     = LT
nmspcmp []     _      = GT
nmspcmp (a:as) (b:bs) = if c == EQ then arraycmp as bs else c
  where c = arraycmp a b

-- Compares two DataTypes in accordance with Section VI.B.2 of the Advanced
-- Assembly 0.5.1 Specification
cmpdt :: DataType -> DataType -> ANmsp -> Ordering
cmpdt (_,BString a)     (_,BString b)     _ = arraycmp a b
cmpdt (_,BInteger a)    (_,BInteger b)    _ = compare a b
cmpdt (_,BRational a b) (_,BRational c d) _ = compare (a%b) (c%d)
cmpdt a@(_,BNmspId _)   b@(_,BNmspId _)   s = nmspcmp av bv
  where av = gnmsp s a
        bv = gnmsp s b
cmpdt (a,BStatement)    (b,BStatement)    _ = arraycmp a b

-- Converts a DataType to a Bool in accordance with Section IV of the Advanced
-- Assembly 0.5.1 Specification
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

-- Converts a Bool to a DataType in accordance with Section IV of the Advanced
-- Assembly 0.5.1 Specification
boolToDT :: Primitive -> Bool -> DataType
boolToDT (BString _)     False = bsToDT []
boolToDT (BString _)     True  = bsToDT [F,F,F,F]
boolToDT (BInteger _)    False = intToDT 0
boolToDT (BInteger _)    True  = intToDT 1
boolToDT (BRational _ _) False = rtlToDT 0 1
boolToDT (BRational _ _) True  = rtlToDT (-1) (-1)
boolToDT (BNmspId _)     False = (p,BNmspId $ Left [])
  where p = (o "AN")++(o "EN")++repeat Terminate
        o t = opcodes M.! t
boolToDT (BNmspId _)     True  = (p,BNmspId $ Left [[]])
  where p = (o "AN")++(o "CN")++(o "ES")++(o "EN")++repeat Terminate
        o t = opcodes M.! t
boolToDT (BStatement)    False = (p,BStatement)
  where p = (o "LS")++(o "LT")++(o "ES")++repeat Terminate
        o t = opcodes M.! t
boolToDT (BStatement)    True  = (p,BStatement)
  where p = (o "LS")++(o "LT")++(o "CS")++[F,F,F,F]++(o "ES")++repeat Terminate
        o t = opcodes M.! t

-- String Utilities
lstring = snd . snd . pstring

cstring :: DataType -> DataType
cstring a@(_,BString _) = a
cstring (b,_) = (b,lstring b)

bsToString :: BitSeries -> BitSeries
bsToString a = foldr (\c a -> (o "CS")++c++a) (o "ES") $ chunksOf 4 a
  where o t = opcodes M.! t

bsToDT :: BitSeries -> DataType
bsToDT s = (bsToString s++repeat Terminate, BString s)

-- Integer Utilities
linteger = snd . snd . pinteger

cinteger :: DataType -> DataType
cinteger a@(_,BInteger _) = a
cinteger (b,_) = (b,linteger b)

intToBin :: Integer -> BitSeries
intToBin 0 = []
intToBin x = let (i,bs) = fbit x [] in bs++intToBin i
  where fbit i s | length s == 4 = (i,s)
                 | otherwise     = fbit (i`div`2) (b:s)
                 where b = if (i`mod`2==0)/=(i<0) then F else T

intToBS :: Integer -> BitSeries
intToBS i = [s]++bsToString (intToBin i)
  where s=if i<0 then T else F

intToDT :: Integer -> DataType
intToDT i = (intToBS i++repeat Terminate,BInteger i)

-- Rational Utilities
lrational = snd . snd . prational

crational :: DataType -> DataType
crational a@(_,BRational _ _) = a
crational (b,_) = (b,lrational b)

rtlToBS :: Integer -> Integer -> BitSeries
rtlToBS a b = (intToBS a)++(intToBS b)
rtlToBS' :: Rational -> BitSeries
rtlToBS' a = rtlToBS (numerator a) (denominator a)

rtlToDT :: Integer -> Integer-> DataType
rtlToDT a b = (rtlToBS a b++repeat Terminate,BRational a b)
rtlToDT' :: Rational -> DataType
rtlToDT' a = rtlToDT (numerator a) (denominator a)

-- Namespace Utilities
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

-- Statement Utilities
cstmt :: DataType -> DataType
cstmt = second (const BStatement)
