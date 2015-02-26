{-# LANGUAGE PolyKinds, TypeOperators #-}
module Control.Wakarusa.Functor1 where

import Control.Natural
import Control.Applicative
import Control.Monad.Normal

import Control.Wakarusa.Pointed1

---------------------------------------------------------------------------
-- | `Functor1` is the Natural Transformation version of Functor. Techically, it *is* a functor.

class Functor1 h where
 fmap1 :: (f :~> g) -> (h f :~> h g)

instance Functor1 FUNCTOR where
 fmap1 o = Nat $ foldNF $ \ x_a tx -> fmap x_a (point1 $$ o $$ tx)

instance Functor1 APPLICATIVE where
 fmap1 o = Nat $ foldNAF pure $ \ ryz ty -> ryz <*> liftNAF (o $$ ty)

instance Functor1 MONAD where
  fmap1 o = Nat $ foldNM return $ \ tx x_r -> liftNM (o $$ tx) >>= x_r

