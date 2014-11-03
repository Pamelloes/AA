-- This module contains functions for parsing data types in Advanced Assembly
module Primitives where
import Opcodes

-- NOTE: The program is assumed to be an infinitely long list (see language
-- specification). If a non-infinite list is provided, behavior is undefined.

type BString = [Bool]
type BInt = Integer

-- Result Program has BString removed.
lstring :: Program -> (Program,BString)
lstring [] = ([],[])
lstring s
  | snd es == True = (fst es,[])
  | snd cs == True = let (a,b)=lstring prog in (a,str++b)
  where es=hasOpcode s "ES"
        cs=hasOpcode s "CS"
        (str,prog) = splitAt 4 (fst cs)

bsToInt :: BString -> Integer
bsToInt [] = 0
bsToInt x = val + 16*(bsToInt remainder)
  where (base,remainder) = splitAt 4 x
        val = foldl (\x y->2*x+(if y then 1 else 0)) 0 base

-- Result Program has BInt removed.
linteger :: Program -> (Program,BInt)
linteger [] = ([],0)
linteger (sign:remainder) = (prog,sgn $ bsToInt str)
  where (prog,str)=lstring remainder
        sgn=if sign then (\x->x-2^(length str)) else (\x->x)
