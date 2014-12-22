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
module BitSeries where

-- A Bit can be either T (True) or F (False). Terminate is used in infinite
-- lists to indicate the end of non-repeating data.
data Bit = T | F | Terminate deriving Show
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

type BitSeries=[Bit]
