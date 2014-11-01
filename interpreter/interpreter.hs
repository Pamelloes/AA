-- This is the interpreter for Advanced Assembly version 0.4
import qualified  Data.Map as M
import Opcodes

hasOpcode :: [Bool] -> [Char] -> Bool
hasOpcode [] _ = False
hasOpcode _ [] = False
hasOpcode s k 
  | length z==length pattern = fst (foldl (\(a,_) (x,y) -> (a&&x==y,False))
                               (True,True) z) == True
  | otherwise = False
  where pattern=opcodes M.! k
        z = zip pattern s

main = do
  print (hasOpcode [True,False,True] "ES")
  print (hasOpcode [True,False,True] "CS")
  print (hasOpcode [True,False,True] "ERN")
  print (hasOpcode [True,False,True] "LI")
  print (hasOpcode [True,False,True] "BLE")
  print (hasOpcode [True,False,True,True] "BLE")
