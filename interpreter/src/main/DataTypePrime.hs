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
-- This module consists of an alternate implementation of the various primitives
-- in the hope of creating a more elegant parsing system.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DataTypePrime where

import BitSeries
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Writer.Lazy
import qualified Data.Data as D
import qualified Data.Map as M
import Data.Typeable
import Opcodes
import Text.Parsec.Combinator
import Text.Parsec.Prim (ParsecT, Parsec)

class Primitive a where
  convert :: BitSeries -> a

data DataType a = DataType { bs::BitSeries, prim::a }
instance Functor DataType where
  fmap f (DataType a b) = DataType a (f b)
instance Applicative DataType where
  pure = DataType []
  a <*> b = DataType (bs a <> bs b) (prim a $ prim b)
instance Monad DataType where
  return = pure
  a >>= f = let b = f (prim a) in DataType (bs a <> bs b) (prim b)
instance MonadWriter BitSeries DataType where
  tell s = DataType s ()
  listen s = DataType (bs s) (prim s,bs s)
  pass s = DataType ((snd $ prim s) (bs s)) (fst $ prim s)
instance (Monoid a) => Monoid (DataType a) where
  mempty = DataType [] (mempty)
  a `mappend` b = DataType (bs a <> bs b) (prim a <> prim b)

ptell :: (MonadWriter a m) => a -> m a
ptell a = tell a >> return a

--convert :: (Primitive b) => DataType a -> DataType b
--convert (DataType a _) = (DataType a $ convert a)

type BString = BitSeries
instance Primitive BString where
  convert = undefined

type BInteger = Integer
instance Primitive BInteger where
  convert = undefined

type BRational = (Integer,Integer)
instance Primitive BRational where
  convert = undefined

data RNmspS = Child BitSeries | Parent deriving (Show,D.Data,Typeable)
type RNmsp = [RNmspS]
type ANmsp = [BitSeries]
type BNmspId = Either ANmsp RNmsp
instance Primitive BNmspId where
  convert = undefined

type BStatement = ()
instance Primitive BStatement where
  convert = undefined

-- Strings
type BSInt u = Parsec BitSeries u (DataType ())
pstring :: Parsec BitSeries u (DataType BString)
pstring = (mconcat <$> (many cs)) <* ((tell <$> mopc "ES")::BSInt u)
  where cs :: Parsec BitSeries u (DataType BitSeries)
        cs = ((tell <$> mopc "CS")::BSInt u)
             >> (ptell <$> replicateM 4 anyToken)

-- Integers
bsToInt :: [Bit] -> Integer
bsToInt [] = 0
bsToInt x = val + 16*(bsToInt remainder)
  where (base,remainder) = splitAt 4 x
        val = foldl (\x y->2*x+(if y==T then 1 else 0)) 0 base

pinteger :: Parsec BitSeries u (DataType BInteger)
pinteger = undefined

-- Rationals
prational :: Parsec BitSeries u (DataType BRational)
prational = undefined 

-- Namespaces
panmsp :: Parsec BitSeries u (BitSeries,ANmsp)
panmsp = undefined

prnmsp :: Parsec BitSeries u (BitSeries,RNmsp)
prnmsp = undefined

pnmsp :: Parsec BitSeries u (DataType BNmspId)
pnmsp = undefined
