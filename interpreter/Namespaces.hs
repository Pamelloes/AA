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
findNamespace (Parent _) Namespace{parent = Nothing} = Nothing
findNamespace (Parent id) Namespace{parent = Just a} = findNamespace id a
findNamespace (Child name id) nmsp = if exists 
  then findNamespace id (children nmsp M.! name)
  else Nothing
    where exists = M.member name $ children nmsp
