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
import qualified Data.Map as M
import DataType
import Opcodes
import Statement

-- Namespace definitions
type Namespaces = M.Map ANmsp DataType

defaultNamespace :: BitSeries -> Namespaces
defaultNamespace p = M.fromList [([],(p,BStatement))]

nmspValue :: ANmsp -> DataType -> Namespaces -> DataType
nmspValue a d n = if M.member i n then n M.! i else (repeat Terminate,BString [])
  where i=gnmsp a d

nmspValueSet :: ANmsp -> DataType -> DataType -> Namespaces -> Namespaces
nmspValueSet a d = M.insert i
  where i = gnmsp a d
