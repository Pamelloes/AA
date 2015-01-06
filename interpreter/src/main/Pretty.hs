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
-- This module provides a stop-gap interpreter that simply prints a program in a
-- more legible form.
module Pretty where

import DataType
import Statement

showProgram :: Free Stmt () -> String
showProgram (Free (LS a)) = "\nLiteral: "++show a
showProgram (Free (AS a b)) = "\nAssign:\nNmsp: "++showProgram a++"\nVal: "
  ++showProgram b
showProgram (Free (RS a)) = "\nRetrieve: "++showProgram a
showProgram (Free (ET a b)) = "\nExecute:\nNmsp: "++showProgram a++snd c
  where c = foldl (\(i,s) a->(i+1,s++"\n("++show (i+1)++") "++showProgram a))
                  (0,"") b
showProgram (Free (SQ a b)) = "\nSequence:\n(1) "++showProgram a++"\b(2) "
  ++showProgram b
showProgram (Free (IF a b c)) = "\nIf: "++showProgram a++"\nThen: "++showProgram b
  ++"\nElse: "++showProgram c
showProgram (Free (DW a b)) = "\nDo: "++showProgram a++"\nWhile: "
  ++showProgram b
showProgram (Free (MSA a b)) = "\nOperation "++a++": "++showProgram b
showProgram (Free (MSB a b c)) = "\nOperation "++a++":\n(1) "++showProgram b
  ++"\n(2) "++showProgram c
showProgram (Free (IOS a)) = "\nI/O: "++showProgram a

pretty :: Free Stmt () -> IO ()
pretty = putStr . showProgram
