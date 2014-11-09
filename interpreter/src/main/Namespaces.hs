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
import Opcodes
import Primitives

data Namespace = Namespace { name :: BString
                           , value :: BString
                           , children :: M.Map BString Namespace
                           , parent :: Maybe Namespace
                           } deriving (Show)
data NmspId = End | Parent NmspId | Child BString NmspId

globalNamespace :: Program -> Namespace
globalNamespace p = Namespace [] p (M.fromList []) Nothing

lanmsp :: Program -> (Program,NmspId)
lanmsp p
  | snd en = (fst en,End)
  | snd cn = let (pr,id)=lanmsp (fst str) in (pr,Child (snd str) id)
  where en=hasOpcode p "EN"
        cn=hasOpcode p "CN"
        str=lstring (fst cn)

lrnmsp :: Program -> (Program,NmspId)
lrnmsp p
  | snd en = (fst en,End)
  | snd cn = let (pr,id)=lrnmsp (fst str) in (pr,Child (snd str) id)
  | snd pn = let (pr,id)=lrnmsp (fst pn) in (pr,Parent id)
  where en=hasOpcode p "ERN"
        cn=hasOpcode p "CN"
        str=lstring (fst cn)
        pn=hasOpcode p "PN"

lnmsp :: Program -> (Program, NmspId)
lnmsp p
  | snd abs = lanmsp (fst abs)
  | snd rel = lrnmsp (fst rel)
  where abs=hasOpcode p "AN"
        rel=hasOpcode p "RN"

findNamespace :: NmspId -> Namespace -> Maybe Namespace
findNamespace End a = Just a
findNamespace (Parent id) n@Namespace{parent = Nothing} = findNamespace id n
findNamespace (Parent id) Namespace{parent = Just a} = findNamespace id a
findNamespace (Child name id) nmsp = if exists 
  then findNamespace id (children nmsp M.! name)
  else Nothing
    where exists = M.member name $ children nmsp

nmspValue :: NmspId -> Namespace -> BString
nmspValue id nmsp = case findNamespace id nmsp of
  Just n -> value n
  Nothing -> []

nmspValueSet :: NmspId -> Namespace -> BString -> Namespace
nmspValueSet End n val = Namespace (name n) val (children n) (parent n)
nmspValueSet (Parent id) n@Namespace{parent = Nothing} v = nmspValueSet id n v
nmspValueSet (Parent id) n@Namespace{parent = Just a} v
  = Namespace (name n) (value n) (children n) $ Just (nmspValueSet id a v)
nmspValueSet (Child nm id) n v
  = Namespace (name n) (value n) achild (parent n)
    where exists = M.member nm $ children n
          child = children n M.! nm 
          nchild = Namespace nm [] (M.fromList []) (Just n) 
          achild = if exists
                   then M.insert nm (nmspValueSet id child v) (children n)
                   else M.insert nm (nmspValueSet id nchild v) (children n)
