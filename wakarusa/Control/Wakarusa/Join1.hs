{-# LANGUAGE PolyKinds, TypeOperators, MultiParamTypeClasses, FunctionalDependencies, ConstraintKinds #-}
module Control.Wakarusa.Join1 where

import Control.Applicative
import Control.Natural
import Control.Category ((>>>))
import Control.Monad.Normal

import Control.Wakarusa.Functor1
import Control.Wakarusa.Pointed1

---------------------------------------------------------------------------
-- | Join1 is the Natural Transformation version of monadic join

class (Pointed1 h, Functor1 h) => Join1 h c | h -> c where
  join1 :: c g => h g :~> g

instance Join1 FUNCTOR Functor where
  join1 = Nat $ foldNF $ \ x_a tx -> fmap x_a tx

instance Join1 APPLICATIVE Applicative where
  join1 = Nat $ foldNAF pure $ \ ryz ty -> ryz <*> ty

instance Join1 MONAD Monad where
  join1 = Nat $ foldNM return $ \ tx x_r -> tx >>= x_r
    
-- generic run, inside a context
run1 :: Join1 h c => (c g) => (f :~> g) -> h f :~> g
run1 o = fmap1 o >>> join1

