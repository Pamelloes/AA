-- This module contains a list of the valid opcodes in Advanced Assembly and
-- functions for parsing them.
module Opcodes where
import qualified Data.Map as M

-- NOTE: The program is assumed to be an infinitely long list (see language
-- specification). If a non-infinite list is provided, behavior is undefined.

type Program = [Bool]
type Opcode = String

-- If Program starts with Opcode, result Program has the opcode removed.
hasOpcode :: Program -> Opcode -> (Program,Bool)
hasOpcode s k 
  | length z==length pattern = if match then (reduced, True) else (s, False)
  | otherwise = error "hasOpcode: Reached program end"
  where pattern=opcodes M.! k
        z = zip pattern s
        match = fst (foldl (\(a,_) (x,y) -> (a&&x==y,False)) (True,False) z) == True
        reduced = drop (length pattern) s

opcodes=M.fromList
 [("ES",[False])
 ,("CS",[True])

 ,("AN" ,[False])
 ,("RN" ,[True])
 ,("EN" ,[False])
 ,("CN" ,[True])
 ,("ERN",[False,False])
 ,("PN" ,[False,True])

 ,("LS",[False])
 ,("LT",[False])
 ,("LI",[True,False])
 ,("LF",[True,True])
 ,("FS",[True])
 ,("NS",[False])
 ,("AS",[False,False])
 ,("RS",[False,True])
 ,("ET",[True])
 ,("MS",[True])

 ,("OP",[False,False,False,False])
 ,("OM",[False,False,False,True])
 ,("OT",[False,False,True,False])
 ,("OD",[False,False,True,True])
 ,("OE",[False,True,False,False])
 ,("OU",[False,True,False,True])

 ,("BN" ,[False,True,True,False])
 ,("BO" ,[False,True,True,True,False])
 ,("BX" ,[False,True,True,True,True])
 ,("BA" ,[True,False,False,False])
 ,("BE" ,[True,False,False,True])
 ,("BL" ,[True,False,True,False])
 ,("BLE",[True,False,True,True])
 ,("BG" ,[True,True,False,False])
 ,("BGE",[True,True,False,True])

 ,("TN" ,[True,True,True,False])
 ,("TO" ,[True,True,True,True,False,False,False])
 ,("TX" ,[True,True,True,True,False,False,True])
 ,("TA" ,[True,True,True,True,False,True,False])
 ,("TL" ,[True,True,True,True,False,True,True])
 ,("TRA",[True,True,True,True,True,False,False])
 ,("TRL",[True,True,True,True,True,False,True])
 ,("TTL",[True,True,True,True,True,True,False])
 ,("TTR",[True,True,True,True,True,True,True])

 ,("RT",[False,False])
 ,("ST",[False,True])
 ,("IF",[True,False])
 ,("EN",[False])
 ,("EL",[True])
 ,("WH",[True,True])
 ]

