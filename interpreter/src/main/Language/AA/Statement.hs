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
-- This module generates a program's AST in accordance with Section VI of the
-- Advanced Assembly 0.5.2 specification.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.AA.Statement where

import qualified Data.Map as M
import Data.List
import qualified Control.Applicative as A
import Control.Monad
import Language.AA.BitSeries
import Language.AA.DataType
import Language.AA.Opcodes
import Text.Parsec.Prim

----------------------------------
--  START OF Free Monad EXCERPT --
----------------------------------

-- The following lines of code have been adapted from the
-- Free Monad package: https://hackage.haskell.org/package/free

{-
Copyright 2008-2013 Edward Kmett

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-}

data Free f n = Free (f (Free f n)) | Pure n 
instance (Functor f) => Functor (Free f) where
 fmap f = go where
   go (Pure a)  = Pure (f a)
   go (Free fa) = Free (go A.<$> fa)
instance (Functor f) => A.Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free b = Free $ fmap (fmap a) b
  Free a <*> b = Free $ fmap (A.<*> b) a
instance (Functor f) => Monad (Free f) where
  return = Pure
  (Free x) >>= f = Free (fmap (>>= f) x)
  (Pure r) >>= f = f r

instance (Eq (f (Free f a)), Eq a) => Eq (Free f a) where
  Pure a == Pure b = a == b
  Free fa == Free fb = fa == fb
  _ == _ = False
instance (Show (f (Free f a)), Show a) => Show (Free f a) where
  showsPrec d (Pure a) = showParen (d > 10) $
    showString "Pure " . showsPrec 11 a
  showsPrec d (Free m) = showParen (d > 10) $
    showString "Free " . showsPrec 11 m
--------------------------------
--  END OF Free Monad EXCERPT --
--------------------------------

              -- Control Statements
data Stmt next = AS next next
               | RS next
               | ET next [next]
               | SQ next next
               | IF next next next
               | DW next next
              -- Mathematical Statements 
               | MSA Opcode next
               | MSB Opcode next next
              -- IO Statements
               | IOS next
               deriving Show
instance Functor Stmt where
  fmap f (AS a b) = AS (f a) (f b)
  fmap f (RS a) = RS (f a)
  fmap f (ET a b) = ET (f a) (fmap f b)
  fmap f (SQ a b) = SQ (f a) (f b)
  fmap f (IF a b c) = IF (f a) (f b) (f c)
  fmap f (DW a b) = DW (f a) (f b)
  fmap f (MSA z a) = MSA z (f a)
  fmap f (MSB z a b) = MSB z (f a) (f b)
  fmap f (IOS a) = IOS (f a)
type DStmt = (DataType,Free Stmt DataType)

dpre :: BitSeries -> (BitSeries,DStmt) -> (BitSeries,DStmt)
dpre a (b,((c,BStatement),t)) = (b,((a++c,BStatement),t))

loadLS :: Parsec BitSeries u DStmt
loadLS = do
  let mkf :: Opcode -> Parsec BitSeries u DataType -> Parsec BitSeries u DStmt
      mkf o p = do {
        op <- try $ mopc o;
        dt <- p;
        return$((op++fst dt,BStatement),Pure dt);
      }
  let lt = mkf "LT" pstring
  let li = mkf "LI" pinteger
  let lr = mkf "LR" prational
  let ln = mkf "LN" pnmsp
  let lm = mkf "LM" (fmap fst loadStmt)
  lt <|> li <|> lr <|> ln <|> lm

loadTS :: Parsec BitSeries u DStmt
loadTS = do
  let as = do {
    as <- try $ mopc "AS";
    ((b1,BStatement),f1) <- loadStmt;
    ((b2,BStatement),f2) <- loadStmt;
    return $ ((as++b1++b2,BStatement),Free (AS f1 f2));
  }
  let rs = do {
    rs <- try $ mopc "RS";
    ((b,BStatement),f) <- loadStmt;
    return $ ((rs++b,BStatement),Free (RS f));
  }
  let sq = do {
    sq <- try $ mopc "SQ";
    ((b1,BStatement),f1) <- loadStmt;
    ((b2,BStatement),f2) <- loadStmt;
    return $ ((sq++b1++b2,BStatement),Free (SQ f1 f2));
  }
  let iff = do {
    iff <- try $ mopc "IF";
    ((b1,BStatement),f1) <- loadStmt;
    ((b2,BStatement),f2) <- loadStmt;
    ((b3,BStatement),f3) <- loadStmt;
    return $ ((iff++b1++b2++b3,BStatement),Free (IF f1 f2 f3));
  }
  let dw = do {
    dw <- try $ mopc "DW";
    ((b1,BStatement),f1) <- loadStmt;
    ((b2,BStatement),f2) <- loadStmt;
    return $ ((dw++b1++b2,BStatement),Free (DW f1 f2));
  }
  let et = do {
    et <- try $ mopc "ET";
    ((b1,BStatement),f1) <- loadStmt;
    (b2,BInteger i) <- pinteger;
    ps <- sequence (genericReplicate (abs i) loadStmt);
    let {b3 = foldr ((++).fst.fst) [] ps};
    return $ ((et++b1++b2++b3,BStatement),Free (ET f1 $ fmap (snd) ps));
  }
  as <|> rs <|> sq <|> iff <|> dw <|> et

abomap =
  [ ("OP" ,True )
  , ("OM" ,True )
  , ("OT" ,True )
  , ("OD" ,True )
  , ("OE" ,True )
  , ("OU" ,True )
  , ("BN" ,False)
  , ("BO" ,True )
  , ("BX" ,True )
  , ("BA" ,True )
  , ("BE" ,True )
  , ("BL" ,True )
  , ("BLE",True )
  , ("BG" ,True )
  , ("BGE",True )
  , ("TN" ,False)
  , ("TO" ,True )
  , ("TX" ,True )
  , ("TA" ,True )
  , ("TH" ,True )
  , ("TR" ,True )
  ]
loadMS :: Parsec BitSeries u DStmt
loadMS = do
  let mtch :: (Opcode,Bool) -> Parsec BitSeries u DStmt
      mtch (o,mi) = do {
        op <- try $ mopc o;
        ((b1,BStatement),f1) <- loadStmt;
        if mi then do {
          ((b2,BStatement),f2) <- loadStmt;
          return ((op++b1++b2,BStatement),Free (MSB o f1 f2));
        } else return ((op++b1,BStatement),Free (MSA o f1));
      }
  let ops = fmap (mtch) abomap
  foldr1 (<|>) ops

loadFS :: Parsec BitSeries u DStmt
loadFS = do
  let ts = do {
    ts <- try $ mopc "TS";
    ((b,BStatement),f) <- loadTS;
    return ((ts++b,BStatement),f);
  }
  let ms = do {
    ms <- try $ mopc "MS";
    ((b,BStatement),f) <- loadMS;
    return ((ms++b,BStatement),f);
  }
  ts <|> ms

loadIO :: Parsec BitSeries u DStmt
loadIO = do
  (dt,f) <- loadStmt
  return (dt,Free (IOS f))

loadStmt :: Parsec BitSeries u DStmt
loadStmt = do
  let ls = do {
    ls <- try $ mopc "LS";
    ((b,BStatement),f) <- loadLS;
    return ((ls++b,BStatement),f);
  }
  let fs = do {
    fs <- try $ mopc "FS";
    ((b,BStatement),f) <- loadFS;
    return ((fs++b,BStatement),f);
  }
  let io = do {
    io <- try $ mopc "IO";
    ((b,BStatement),f) <- loadIO;
    return ((io++b,BStatement),f);
  }
  ls <|> fs <|> io
