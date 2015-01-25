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
-- This is the main module for the Advanced Assembly library. It exports all
-- major functions and datatypes, as well as provides a few utility functions.
module Language.AA.AdvancedAssembly (
    module Language.AA.BitSeries,
    module Language.AA.Opcodes,
    module Language.AA.DataType,
    module Language.AA.DataType.Util,
    module Language.AA.Statement,
    module Language.AA.Evaluate,
    runProgram,
    runProgram'
  ) where

import Control.Monad
import Language.AA.BitSeries
import Language.AA.Opcodes
import Language.AA.DataType
import Language.AA.DataType.Util
import Language.AA.Statement
import Language.AA.Evaluate

runProgram :: (Monad m) => (DataType -> m DataType) -> BitSeries -> m DataType
runProgram i p = liftM (snd) $ runProgram' (Just . id) i p

runProgram' :: (Monad m) => (State m -> Maybe (State m)) 
             -> (DataType -> m DataType) -> BitSeries -> m (State m, DataType)
runProgram' j i p = evaluate f s
  where f = snd $ parseST loadStmt p
        s = S ([],(defaultNamespace p,(i,j)))
