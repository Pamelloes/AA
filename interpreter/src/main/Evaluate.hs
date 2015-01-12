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
-- This module evaluates a program's AST in accordance with Sections VI and VII
-- of the Advanced Assembly 0.5.1 specification.
module Evaluate where

import BitSeries
import Control.Arrow
import Data.Char
import qualified Data.Data as D
import Data.Fixed
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Ratio
import DataType
import Opcodes
import Statement

type State = (ANmsp,Namespaces)

-- Namespace definitions
type Namespaces = M.Map ANmsp DataType

defaultNamespace :: BitSeries -> Namespaces
defaultNamespace p = M.fromList [([],(p,BStatement))]

nmspValue :: ANmsp -> DataType -> Namespaces -> DataType
nmspValue a b =  M.findWithDefault (repeat Terminate,BString []) (gnmsp a b)

nmspValue' :: State -> DataType -> (State,DataType)
nmspValue' a b = (a,nmspValue (fst a) b (snd a))

nmspValueSet :: ANmsp -> DataType -> DataType -> Namespaces -> Namespaces
nmspValueSet a b = M.insert $ gnmsp a b

nmspValueSet' :: State -> DataType -> DataType -> (State,DataType)
nmspValueSet' a b c = (s a,c)
  where s = second . const $ nmspValueSet (fst a) b c (snd a)

-- Utilities
cstmt :: DataType -> DataType
cstmt a@(_,BStatement) = a
cstmt (a,_) = (a,BStatement)

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

cmpdt :: DataType -> DataType -> ANmsp -> Ordering
cmpdt (_,BString a)     (_,BString b)     _ = arraycmp a b
cmpdt (_,BInteger a)    (_,BInteger b)    _ = compare a b
cmpdt (_,BRational a b) (_,BRational c d) _ = compare (a%b) (c%d)
cmpdt a@(_,BNmspId _)   b@(_,BNmspId _)   s = nmspcmp av bv
  where av = gnmsp s a
        bv = gnmsp s b
cmpdt (a,BStatement)    (b,BStatement)    _ = arraycmp a b

bsToString :: BitSeries -> BitSeries
bsToString a = foldr (\c a -> (o "CS")++c++a) (o "ES") $ chunksOf 4 a
  where o t = opcodes M.! t

bsToString' :: BitSeries -> DataType
bsToString' s = (bsToString s++repeat Terminate, BString s)

fbit :: Integer -> [Bit] -> (Integer,[Bit])
fbit i s
  | length s == 4 = (i,s)
  | otherwise     = fbit (i`div`2) (b:s)
  where b = if (i`mod`2==0)/=(i<0) then F else T

intToBS :: Integer -> [Bit]
intToBS 0 = []
intToBS x = let (i,bs) = fbit x [] in bs++intToBS i

intToBS' :: Integer -> BitSeries
intToBS' i = [s]++bsToString (intToBS i)
  where s=if i<0 then T else F

intToDT :: Integer -> DataType
intToDT i = (intToBS' i++repeat Terminate,BInteger i)

rtlToBS :: Rational -> BitSeries
rtlToBS a = rtlToBS' (numerator a) (denominator a)

rtlToBS' :: Integer -> Integer -> BitSeries
rtlToBS' a b = (intToBS' a)++(intToBS' b)

rtlToDT :: Rational -> DataType
rtlToDT a = (rtlToBS a++repeat Terminate,BRational (numerator a) (denominator a))

rtlToDT' :: Integer -> Integer-> DataType
rtlToDT' a b = (rtlToBS' a b++repeat Terminate,BRational a b)

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
boolToDT (BString _)     False = bsToString' []
boolToDT (BString _)     True  = bsToString' [F,F,F,F]
boolToDT (BInteger _)    False = intToDT 0
boolToDT (BInteger _)    True  = intToDT 1
boolToDT (BRational _ _) False = rtlToDT (0%1)
boolToDT (BRational _ _) True  = rtlToDT ((-1)%(-1))
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

-- Operations
prlst = [ D.toConstr $ BStatement, D.toConstr $ BNmspId $ Left []
        , D.toConstr $ BString [], D.toConstr $ BInteger 0
        , D.toConstr $ BRational 0 0
        ]
prcnv = [ crational, cinteger, cstring, cnmsp, cstmt ]
prior :: Primitive -> Int
prior p = maybe (error "Unknown Primitive constructor!") id $ 
  elemIndex (D.toConstr p) prlst
prior' :: DataType -> Int
prior' = prior . snd

ensureMin :: Int -> DataType -> DataType
ensureMin a b = if a > (prior' b) then (prcnv!!a) b else b
ensureMin' :: Primitive -> DataType -> DataType
ensureMin' a = ensureMin (prior a)

normDt :: DataType -> DataType -> (DataType,DataType)
normDt a b = (ensureMin m a, ensureMin m b)
  where m = max (prior' a) (prior' b)

normMDt :: Primitive -> DataType -> DataType -> (DataType,DataType)
normMDt a b = normDt (ensureMin' a b)

evaluateMSB :: Free Stmt () -> State -> IO (State,DataType)
evaluateMSB (Free (MSB p a b)) s = do
  (s2,av) <- evaluate a s
  (s3,bv) <- evaluate b s2
  let v=case p of
          "OP" -> case (normMDt (BString []) av bv) of
            ((_,BString t),(_,BString u)) -> bsToString' (t++u)
            ((_,BInteger t),(_,BInteger u)) -> intToDT (t+u)
            ((_,BRational t u),(_,BRational v w)) -> if u==0||w==0 then 
              rtlToDT' 0 0 else rtlToDT ((t%u)+(v%w))
          "OM" -> case (normMDt (BInteger 0) av bv) of
            ((_,BInteger t),(_,BInteger u)) -> intToDT (t-u)
            ((_,BRational t u),(_,BRational v w)) -> if u==0||w==0 then 
              rtlToDT' 0 0 else rtlToDT ((t%u)-(v%w))
          "OT" -> case (normMDt (BInteger 0) av bv) of
            ((_,BInteger t),(_,BInteger u)) -> intToDT (t*u)
            ((_,BRational t u),(_,BRational v w)) -> if u==0||w==0 then 
              rtlToDT' 0 0 else rtlToDT ((t%u)*(v%w))
          "OD" -> case (normMDt (BInteger 0) av bv) of
            ((_,BInteger t),(_,BInteger u)) -> if u==0 then
              rtlToDT' 0 0 else intToDT $ t`div`u
            ((_,BRational t u),(_,BRational v w)) -> if u==0||w==0||v==0 then 
              rtlToDT' 0 0 else rtlToDT ((t%u)/(v%w))
          "OE" -> case (ensureMin' (BInteger 0) av,cinteger bv) of
            ((_,BInteger t),(_,BInteger u)) -> intToDT (t^(abs$u))
            ((_,BRational t u),(_,BInteger v)) -> if u==0 then 
              rtlToDT' 0 0 else rtlToDT ((t%u)^^v)
          "OU" -> case (normMDt (BInteger 0) av bv) of
            ((_,BInteger t),(_,BInteger u)) -> if u==0 then
              rtlToDT' 0 0 else intToDT $ abs (t`rem`u)
            ((_,BRational t u),(_,BRational v w)) -> if u==0||w==0||v==0 then 
              rtlToDT' 0 0 else rtlToDT ((abs$t%u)`mod'`(abs$v%w))
          "BO" -> boolToDT (snd av') $ (dtToBool av') || (dtToBool bv')
            where (av',bv')=normDt av bv
          "BX" -> boolToDT (snd av') $ (dtToBool av') /= (dtToBool bv')
            where (av',bv')=normDt av bv
          "BA" -> boolToDT (snd av') $ (dtToBool av') && (dtToBool bv')
            where (av',bv')=normDt av bv
          "BE" -> boolToDT (snd av') $ (cmpdt av' bv' $ fst s3)==EQ
            where (av',bv')=normDt av bv
          "BL" -> boolToDT (snd av') $ (cmpdt av' bv' $ fst s3)==LT
            where (av',bv')=normDt av bv
          "BLE" -> boolToDT (snd av') $ (c==LT)||(c==EQ)
            where (av',bv')=normDt av bv
                  c=(cmpdt av' bv' $ fst s3)
          "BG" -> boolToDT (snd av') $ (cmpdt av' bv' $ fst s3)==GT
            where (av',bv')=normDt av bv
          "BGE" -> boolToDT (snd av') $ (c==GT)||(c==EQ)
            where (av',bv')=normDt av bv
                  c=(cmpdt av' bv' $ fst s3)
  return (s3,v)
simpleOP a b = evaluate a b

-- Stopgap I/O handler
handleIO :: DataType -> IO DataType
handleIO a@(_,BString s) = lg a s
  where lg :: DataType -> BitSeries -> IO DataType
        lg a [] = do
          putChar '\n'
          return a
        lg d s = do
          let (a,b) = splitAt 8 s
          putChar $ chr (fromIntegral $ bsToInt a)
          lg d b
handleIO d = handleIO (cstring d)

-- Evaluate definitions
evaluate :: Free Stmt () -> State -> IO (State,DataType)
evaluate (Free (LS a))        s = return (s,a)
evaluate (Free (AS a b))      s = do
  (s2,av) <- evaluate a s
  (s3,bv) <- evaluate b s2
  return $ nmspValueSet' s3 av bv
evaluate (Free (RS a))        s = do
  (s2,av) <- evaluate a s
  return $ nmspValue' s2 av
evaluate (Free (ET a b))      s = do
  (s2,av) <- evaluate a s
  let n = gnmsp (fst s2) av
  let nid i = n++[intToBS i]
  (s3,_) <- foldl (\v a -> do {
      (s,i)<-v;
      (t,u) <- evaluate a s;
      return ((fst t,M.insert (nid i) u (snd t)),i+1)
    }) (return (s2,1)) b
  let (_,(_,f)) = loadStmt $ fst $ nmspValue (fst s3) av (snd s3)
  ((_,nm),d) <- evaluate f (n,snd s3)
  return $ ((fst s,nm),d)
evaluate (Free (SQ a b))      s = do
  (s2,_) <- evaluate a s
  evaluate b s2
evaluate (Free (IF a b c))    s = do
  (s2,av) <- evaluate a s
  if dtToBool av
    then evaluate b s2
    else evaluate c s2
evaluate (Free (DW a b))      s = do
  let dw s = do {
    (s2,v) <- evaluate a s; 
    (s3,c) <- evaluate b s2;
    if dtToBool c then dw s3 else return (s3,v)
  }
  dw s
evaluate (Free (IOS a))       s = do
  (s2,av) <- evaluate a s
  r <- handleIO av
  return $ (s2,r)
evaluate (Free (MSA p a))     s = do
  (s2,av) <- evaluate a s
  let v=case p of
          "BN" -> boolToDT (snd av) . not . dtToBool $ av
  return (s2,v)
evaluate a@(Free (MSB _ _ _)) s = evaluateMSB a s
