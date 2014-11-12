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
-- This module provides a zipper for manipulating lists.
module LZipper where

-- First is focus, Second is trail
type LZipper a = ([a],[a])

forward :: LZipper a -> Maybe (LZipper a)
forward (l:ls,bs) = Just (ls,l:bs)
forward ([],_) = Nothing

backward :: LZipper a -> Maybe (LZipper a)
backward (ls,c:bs) = Just (c:ls,bs)
backward (_,[]) = Nothing

zipper :: [a] -> LZipper a
zipper l = (l,[])

unzipper :: LZipper a -> [a]
unzipper (ls,bs) = reverse bs ++ ls

end :: LZipper a -> LZipper a
end (ls,bs) = ([],reverse ls ++ bs)

start :: LZipper a -> LZipper a
start (ls,bs) = (reverse bs ++ ls,[])
