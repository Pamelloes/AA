-- This is the interpreter for Advanced Assembly version 0.4
import qualified  Data.Map as M
import Opcodes

import Debug.Trace

-- NOTE: The program is assumed to be an infinitely long list (see language
-- specification). If a non-infinite list is provided, behavior is undefined.

-- (program,opcode) -> (program',hasOpcode)
-- If program starts with opcode, program' has the opcode removed.
-- Otherwise, program'=program
hasOpcode :: [Bool] -> [Char] -> ([Bool],Bool)
hasOpcode [] _ = ([],False)
hasOpcode s k 
  | length z==length pattern && match = (reduced, True)
  | otherwise = (s,False)
  where pattern=opcodes M.! k
        z = zip pattern s
        match = fst (foldl (\(a,_) (x,y) -> (a&&x==y,False)) (True,False) z) == True
        reduced = drop (length pattern) s

-- Program -> (Program',String); Program' is program with the string removed.
lstring :: [Bool] -> ([Bool],[Bool])
lstring [] = ([],[])
lstring s
  | snd es == True = (fst es,[])
  | snd cs == True = let (a,b)=lstring prog in (a,str++b)
  where es=hasOpcode s "ES"
        cs=hasOpcode s "CS"
        (str,prog) = splitAt 4 (fst cs)

main = do
  print (lstring [True,True,False,True,False,True,False,False,False,False,True,
         True,True,True,True,False])
