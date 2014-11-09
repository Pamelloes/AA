-- This is the interpreter for Advanced Assembly version 0.4
module Main where

import Primitives

-- NOTE: The program is assumed to be an infinitely long list (see language
-- specification). If a non-infinite list is provided, behavior is undefined.

main = do
  print (linteger $ True:(concat (replicate 32 [True,True,True,True,True]))
                  ++[True,False,True,True,True,False])
