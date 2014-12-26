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
-- This is the interpreter for Advanced Assembly version 0.4
module Main where

import BitSeries
import DataType
import Options.Applicative

data Cmdline = Cmdline
  { file :: String
  }
cmdline :: Parser Cmdline
cmdline = Cmdline <$> strArgument (metavar "file")

run :: Cmdline -> IO ()
run c = do
  print (linteger $ T:(concat (replicate 32 [T,T,T,T,T])) ++[T,F,T,T,T,F])

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> cmdline)
          ( fullDesc 
         <> progDesc "Run the program stored in file"
         <> header "Advanced Assembly 0.5.0 Interpreter"
          )
