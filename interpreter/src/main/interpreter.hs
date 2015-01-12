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
-- This is the interpreter for Advanced Assembly version 0.5.1
module Main where

import BitSeries
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Word
import Evaluate
import Options.Applicative
import Statement

data Cmdline = Cmdline
  { file :: String
  }
cmdline :: Parser Cmdline
cmdline = Cmdline <$> strArgument (metavar "file")

up :: Word8 -> [Bit]
up w = [h,g,f,e,d,c,b,a]
  where z x=if x== 0 then F else T
        a=z$w.&.0x1
        b=z$w.&.0x2
        c=z$w.&.0x4
        d=z$w.&.0x8
        e=z$w.&.0x10
        f=z$w.&.0x20
        g=z$w.&.0x40
        h=z$w.&.0x80


run :: Cmdline -> IO ()
run c = do
  p <- B.readFile $ file c
  let prog = (B.foldr (\b ac -> (up b)++ac) [] p)++(repeat Terminate)
  let istate = ([],defaultNamespace prog)
  let mnst = loadStmt prog
  --print (snd mnst) -- We can't print the first part because it's infinite...
  (fstate,res) <- evaluate (snd $ snd mnst) istate
  print res

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> cmdline)
          ( fullDesc 
         <> progDesc "Run the program stored in file"
         <> header "Advanced Assembly 0.5.1 Interpreter"
          )
