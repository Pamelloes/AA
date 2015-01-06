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
-- Advanced Assembly 0.5.1 specification.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Statement where

import BitSeries
import qualified Data.Map as M
import Control.Applicative
import DataType
import Opcodes

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
   go (Free fa) = Free (go <$> fa)
instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free b = Free $ fmap (fmap a) b
  Free a <*> b = Free $ fmap (<*> b) a
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

data Stmt next = LS DataType
              -- Control Statements
               | AS next next
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
  fmap f (LS z) = LS z
  fmap f (AS a b) = AS (f a) (f b)
  fmap f (RS a) = RS (f a)
  fmap f (ET a b) = ET (f a) (fmap f b)
  fmap f (SQ a b) = SQ (f a) (f b)
  fmap f (IF a b c) = IF (f a) (f b) (f c)
  fmap f (DW a b) = DW (f a) (f b)
  fmap f (MSA z a) = MSA z (f a)
  fmap f (MSB z a b) = MSB z (f a) (f b)
  fmap f (IOS a) = IOS (f a)
type DStmt a = (DataType,Free Stmt ())

dpre :: BitSeries -> (BitSeries,DStmt a) -> (BitSeries,DStmt a)
dpre a (b,((c,BStatement),t)) = (b,((a++c,BStatement),t))

loadLS :: BitSeries -> (BitSeries,DStmt n)
loadLS s 
  | snd lt = pr "LT" $ pstring (fst lt)
  | snd li = pr "LI" $ pinteger (fst li)
  | snd lr = pr "LR" $ prational (fst lr)
  | snd ln = pr "LN" $ pnmsp (fst ln)
  | snd lm = pr "LM" $ let (b,(d,_))=loadStmt (fst lm) in (b,d)
  where lt = hasOpcode s "LT"
        li = hasOpcode s "LI"
        lr = hasOpcode s "LR"
        ln = hasOpcode s "LN"
        lm = hasOpcode s "LM"
        pr :: Opcode -> (BitSeries,DataType) -> (BitSeries,DStmt n)
        pr o (t,d) = (t,ds)
          where p=(opcodes M.! o)++(fst d)
                dt=(p,BStatement)
                ds=(dt,Free (LS d))

loadTS :: BitSeries -> (BitSeries, DStmt a)
loadTS s
  | snd as = let s1 = loadStmt $ fst as in
    let s2 = loadStmt $ fst s1 in
      (fst s2,((aso++bts s1++bts s2,BStatement),Free (AS (btf s1) (btf s2))))
  | snd rs = let s1 = loadStmt $ fst rs in
    (fst s1,((rso++bts s1,BStatement),Free (RS (btf s1))))
  | snd et = undefined
  | snd sq = let s1 = loadStmt $ fst sq in
    let s2 = loadStmt $ fst s1 in
      (fst s2,((sqo++bts s1++bts s2,BStatement),Free (SQ (btf s1) (btf s2))))
  | snd iff = let s1 = loadStmt $ fst iff in
    let s2 = loadStmt $ fst s1 in
      let s3 = loadStmt $ fst s2 in
        (fst s3,((ifo++bts s1++bts s2++bts s3,BStatement),
          Free (IF (btf s1) (btf s2) (btf s3))))
  | snd dw = let s1 = loadStmt $ fst dw in
    let s2 = loadStmt $ fst s1 in
      (fst s2,((dwo++bts s1++bts s2,BStatement),Free (DW (btf s1) (btf s2))))
  where bts = fst . fst .snd
        btf = snd . snd
        as = hasOpcode s "AS"
        aso = opcodes M.! "AS"
        rs = hasOpcode s "RS"
        rso = opcodes M.! "RS"
        et = hasOpcode s "ET"
        eto = opcodes M.! "ET"
        sq = hasOpcode s "SQ"
        sqo = opcodes M.! "SQ"
        iff = hasOpcode s "IF"
        ifo = opcodes M.! "IF"
        dw = hasOpcode s "DW"
        dwo = opcodes M.! "DW"

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
  , ("TS" ,True )
  , ("TR" ,True )
  ]
loadMS :: BitSeries -> (BitSeries, DStmt a)
loadMS bs
  | b = (fst s2,((os++s1b++s2b,BStatement),Free (MSB o s1s s2s)))
  | otherwise = (fst s1,((os++s1b,BStatement),Free (MSA o s1s)))
  where (s,o,b) = opc bs abomap
        os = opcodes M.! o
        s1 = loadStmt s
        s1b = fst $ fst $ snd s1
        s1s = snd $ snd s1
        s2 = loadStmt $ fst s1
        s2b = fst $ fst $ snd s2
        s2s = snd $ snd s2
        opc :: BitSeries -> [(String,Bool)] -> (BitSeries,String,Bool)
        opc _ [] = error "No matched opcode!"
        opc bt (o:os) = if snd res then (fst res,fst o,snd o) else opc bt os
          where res = hasOpcode bt (fst o)

loadFS :: BitSeries -> (BitSeries, DStmt a)
loadFS s
  | snd ts = dpre tso $ loadTS $ fst ts
  | snd ms = dpre mso $ loadMS $ fst ms
  where ts = hasOpcode s "TS"
        tso = opcodes M.! "TS"
        ms = hasOpcode s "MS"
        mso = opcodes M.! "MS"

loadIO :: BitSeries -> (BitSeries, DStmt a)
loadIO b = let (e,(d,s))=loadStmt b in (e,(d,Free (IOS s)))

loadStmt :: BitSeries -> (BitSeries, DStmt a)
loadStmt s
  | snd ls = dpre lso $ loadLS $ fst ls
  | snd fs = dpre fso $ loadFS $ fst fs
  | snd io = dpre ioo $ loadIO $ fst io
  where ls = hasOpcode s "LS"
        lso = opcodes M.! "LS"
        fs = hasOpcode s "FS"
        fso = opcodes M.! "FS"
        io = hasOpcode s "IO"
        ioo = opcodes M.! "IO"
