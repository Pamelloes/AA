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
-- This module contains functions used to generate test cases.
module TestUtil where

import qualified Data.Map as M
import Language.AA.BitSeries
import Language.AA.Opcodes
import Test.HUnit
import Text.Parsec.Error
import Text.Parsec.Prim

instance Eq ParseError where
  a == b = (errorPos a == errorPos b) && (errorMessages a == errorMessages b)

o :: Opcode -> BitSeries
o = (M.!) opcodes 

etest :: (Show a, Eq a) => String -> a -> a -> Test
etest s a b = TestLabel s $ TestCase $ assertEqual "" a b

ptest :: (Show a, Eq a) => String -> a -> Parsec BitSeries () a -> BitSeries -> Test
ptest s a p b = etest s (Right a) $ parse p "" b

ptestf :: (Show a, Eq a) => String -> Parsec BitSeries () a -> BitSeries -> Test
ptestf s p b = TestLabel s $ TestCase $ case (parse p "" b) of
  Left  _ -> return ()
  Right a -> assertFailure $ "Expected error but got: "++show a
