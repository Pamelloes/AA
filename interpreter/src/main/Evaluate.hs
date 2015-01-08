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
import Data.List
import qualified Data.Map as M
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
intToBS :: Integer -> [Bit]
intToBS 0 = []
intToBS x = let (i,bs) = fbit x [] in bs++intToBS i
  where fbit :: Integer -> [Bit] -> (Integer,[Bit])
        fbit i s
          | length s == 4  = (i,s)
          | otherwise       = fbit (i`div`2) (b:s) 
          where b = if i `mod` 2 == 0 then F else T

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

{-
estring   a = first cstring   . (evaluate a)
einteger  a = first cinteger  . (evaluate a)
erational a = first crational . (evaluate a)
enmsp     a = first cnmsp     . (evaluate a)
-}

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
evaluate (Free (LS a))     s = return (s,a)
evaluate (Free (AS a b))   s = do
  (s2,av) <- evaluate a s
  (s3,bv) <- evaluate b s2
  return $ nmspValueSet' s3 av bv
evaluate (Free (RS a))     s = do
  (s2,av) <- evaluate a s
  return $ nmspValue' s2 av
evaluate (Free (ET a b))   s = do
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
evaluate (Free (SQ a b))   s = do
  (s2,_) <- evaluate a s
  evaluate b s2
evaluate (Free (IF a b c)) s = do
  (s2,av) <- evaluate a s
  if dtToBool av
    then evaluate b s2
    else evaluate c s2
evaluate (Free (DW a b))   s = do
  let dw s = do {
    (s2,v) <- evaluate a s; 
    (s3,c) <- evaluate b s2;
    if dtToBool c then dw s3 else return (s3,v)
  }
  dw s
evaluate (Free (IOS a))    s = do
  (s2,av) <- evaluate a s
  r <- handleIO av
  return $ (s2,r)
{-
evaluate (Free (MSA a b)) = "\nOperation "++a++": "++showProgram b
evalute (Free (MSB a b c)) = "\nOperation "++a++":\n(1) "++showProgram b
  ++"\n(2) "++showProgram c
evaluate (Free (IOS a)) = "\nI/O: "++showProgram a
-}
