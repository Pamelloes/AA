-- This module generates a program's AST in accordance with Section VI of the
-- Advanced Assembly 0.5.0 specification.
module Statement where

import DataType
import Opcodes

data Free f n = Free f (Free f n) | Pure n
instance (Functor f) => Monad (Free f) where
  return = Pure
  (Free x) >>= f = Free (fmap (>>= f) x)
  (Pure r) >>= f = f r

data Stmt next = LS DataType
              -- Control Statements
               | AS next next
               | RS next
               | ET [next]
               | SQ next next
               | IF next next next
               | DW next next
              -- Mathematical Statements 
               | MSA Opcode next
               | MSB Opcode next next
              -- IO Statements
               | IOS next
