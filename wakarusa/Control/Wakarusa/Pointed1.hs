{-# LANGUAGE PolyKinds, TypeOperators #-}
module Control.Wakarusa.Pointed1 where

import Control.Natural

import Control.Monad.Normal

---------------------------------------------------------------------------
-- | Pointed1 is the Natural Transformation version of Pointed.

class Pointed1 h where 
  point1 :: g :~> h g

instance Pointed1 FUNCTOR where
  point1 = Nat liftNF

instance Pointed1 APPLICATIVE where
  point1 = Nat liftNAF

instance Pointed1 MONAD where
  point1 = Nat liftNM
