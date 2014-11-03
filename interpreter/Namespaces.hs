-- This module contains functions dealing with namesapces in Advanced Assembly
module Namespaces where
import qualified Data.Map as M
import Opcodes
import Primitives

data Namespace = Namespace { name :: BString
                           , value :: BString
                           , children :: M.Map BString Namespace
                           , parent :: Maybe Namespace
                           } deriving (Show)
data NmspId = End | Parent NmspId | Child BString NmspId

globalNamespace :: Program -> Namespace
globalNamespace p = Namespace [] p (M.fromList []) Nothing

findNamespace :: NmspId -> Namespace -> Maybe Namespace
findNamespace End a = Just a
findNamespace (Parent id) n@Namespace{parent = Nothing} = findNamespace id n
findNamespace (Parent id) Namespace{parent = Just a} = findNamespace id a
findNamespace (Child name id) nmsp = if exists 
  then findNamespace id (children nmsp M.! name)
  else Nothing
    where exists = M.member name $ children nmsp

nmspValue :: NmspId -> Namespace -> BString
nmspValue id nmsp = case findNamespace id nmsp of
  Just n -> value n
  Nothing -> []

nmspValueSet :: NmspId -> Namespace -> BString -> Namespace
nmspValueSet End n val = Namespace (name n) val (children n) (parent n)
nmspValueSet (Parent id) n@Namespace{parent = Nothing} v = nmspValueSet id n v
nmspValueSet (Parent id) n@Namespace{parent = Just a} v
  = Namespace (name n) (value n) (children n) $ Just (nmspValueSet id a v)
nmspValueSet (Child nm id) n v
  = Namespace (name n) (value n) achild (parent n)
    where exists = M.member nm $ children n
          child = children n M.! nm 
          nchild = Namespace nm [] (M.fromList []) (Just n) 
          achild = if exists
                   then M.insert nm (nmspValueSet id child v) (children n)
                   else M.insert nm (nmspValueSet id nchild v) (children n)
