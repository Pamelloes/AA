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
-- This module contains functions dealing with namesapces in Advanced Assembly
module Namespaces where

import qualified Data.Map as M
import LZipper
import Opcodes
import Primitives

type NmspId = [Primitive]
type NZipper = LZipper Primitive
type Namespaces = M.Map NmspId Primitive

defaultNamespace :: Program -> Namespaces
defaultNamespace p = M.fromList [([],BString p)]

lanmsp :: Program -> (Program,NmspId)
lanmsp p
  | snd en = (fst en,[])
  | snd cn = let (pr,id)=lanmsp prog in (pr,str:id)
  where en=hasOpcode p "EN"
        cn=hasOpcode p "CN"
        (prog, str)=lstring (fst cn)

lrnmsp :: NZipper -> Program -> (Program,NmspId)
lrnmsp z p
  | snd en = (fst en,unzipper z)
  | snd cn = lrnmsp (fst z,str:snd z) prog
  | snd pn = lrnmsp (fst z,tail $ snd z) (fst pn)
  where en=hasOpcode p "ERN"
        cn=hasOpcode p "CN"
        (prog, str)=lstring (fst cn)
        pn=hasOpcode p "PN"

lnmsp :: NmspId -> Program -> (Program, NmspId)
lnmsp b p
  | snd abs = lanmsp (fst abs)
  | snd rel = lrnmsp (end $ zipper b) (fst rel)
  where abs=hasOpcode p "AN"
        rel=hasOpcode p "RN"

nmspValue :: NmspId -> Namespaces -> Primitive
nmspValue id nmsp = if M.member id nmsp then nmsp M.! id else BString []

nmspValueSet :: NmspId -> Primitive -> Namespaces -> Namespaces
nmspValueSet = M.insert
