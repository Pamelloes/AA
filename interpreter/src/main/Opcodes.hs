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
-- This module contains a list of the valid opcodes in Advanced Assembly and
-- functions for parsing them.
module Opcodes where

import qualified Data.Map as M

-- A Bit can be either T (True) or F (False). Terminate is used in infinite
-- lists to indicate the end of non-repeating data.
data Bit=T|F|Terminate deriving Show
instance Eq Bit where
  T /= F = True
  F /= T = True
  T /= Terminate = True
  Terminate /= T = True
  _ /= _ = False
instance Ord Bit where
  T `compare` a = if a == F then GT else EQ
  a `compare` T = if a == F then LT else EQ
  Terminate `compare` F = LT
  F `compare` Terminate = GT
  _ `compare` _ = EQ

-- NOTE: The program is assumed to be an infinitely long list (see language
-- specification). If a non-infinite list is provided, behavior is undefined.

type Program = [Bit]
type Opcode = String

-- If Program starts with Opcode, result Program has the opcode removed.
hasOpcode :: Program -> Opcode -> (Program,Bool)
hasOpcode s k 
  | length z==length pattern = if match then (reduced, True) else (s, False)
  | otherwise = error "hasOpcode: Reached program end"
  where pattern=opcodes M.! k
        z = zip pattern s
        match = fst (foldl (\(a,_) (x,y) -> (a&&x==y,False)) (True,False) z) == True
        reduced = drop (length pattern) s

opcodes=M.fromList
 [("ES",[F])
 ,("CS",[T])

 ,("AN" ,[F])
 ,("RN" ,[T])
 ,("EN" ,[F])
 ,("CN" ,[T])
 ,("ERN",[F,F])
 ,("PN" ,[F,T])

 ,("LS",[F])
 ,("LT",[F])
 ,("LI",[T,F])
 ,("LF",[T,T])
 ,("FS",[T])
 ,("NS",[F])
 ,("AS",[F,F])
 ,("RS",[F,T])
 ,("ET",[T])
 ,("MS",[T])

 ,("OP",[F,F,F,F])
 ,("OM",[F,F,F,T])
 ,("OT",[F,F,T,F])
 ,("OD",[F,F,T,T])
 ,("OE",[F,T,F,F])
 ,("OU",[F,T,F,T])

 ,("BN" ,[F,T,T,F])
 ,("BO" ,[F,T,T,T,F])
 ,("BX" ,[F,T,T,T,T])
 ,("BA" ,[T,F,F,F])
 ,("BE" ,[T,F,F,T])
 ,("BL" ,[T,F,T,F])
 ,("BLE",[T,F,T,T])
 ,("BG" ,[T,T,F,F])
 ,("BGE",[T,T,F,T])

 ,("TN" ,[T,T,T,F])
 ,("TO" ,[T,T,T,T,F,F,F])
 ,("TX" ,[T,T,T,T,F,F,T])
 ,("TA" ,[T,T,T,T,F,T,F])
 ,("TL" ,[T,T,T,T,F,T,T])
 ,("TRA",[T,T,T,T,T,F,F])
 ,("TRL",[T,T,T,T,T,F,T])
 ,("TTL",[T,T,T,T,T,T,F])
 ,("TTR",[T,T,T,T,T,T,T])

 ,("RT",[F,F])
 ,("ST",[F,T])
 ,("IF",[T,F])
 ,("EN",[F])
 ,("EL",[T])
 ,("WH",[T,T])
 ]

