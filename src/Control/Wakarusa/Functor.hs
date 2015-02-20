{-# LANGUAGE PolyKinds, TypeOperators, GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.Functor where

import Control.Monad.ConstrainedNormal
import Control.Applicative
import Control.Natural

---------------------------------------------------------------------------
-- | `Functor1` is the high-kinded version of Functor.

class Functor1 h where
 fmap1 :: (f :~> g) -> (h f :~> h g)

---------------------------------------------------------------------------

class Lift f where 
  lift :: g :~> f g

instance Lift FUNCTOR where
  lift = Nat liftNF

instance Lift APPLICATIVE where
  lift = Nat liftNAF

instance Lift MONAD where
  lift = Nat liftNM

---------------------------------------------------------------------------
type FUNCTOR = NF Unconstrained

instance Functor1 FUNCTOR where
 fmap1 o = Nat $ foldNF $ \ x_a tx -> fmap x_a (lift $$ o $$ tx)

---------------------------------------------------------------------------
type APPLICATIVE = NAF Unconstrained

instance Functor1 (NAF c) where
 fmap1 o = Nat $ foldNAF pure $ \ ryz ty -> ryz <*> liftNAF (o $$ ty)

---------------------------------------------------------------------------
type MONAD = NM Unconstrained

instance Functor1 (NM c) where
  fmap1 o = Nat $ foldNM return $ \ tx x_r -> liftNM (o $$ tx) >>= x_r

---------------------------------------------------------------------------
-- Assumes 7.10-like thinking here

f2a :: (Lift g, Functor (g f)) => FUNCTOR f :~> g f
f2a = Nat $ foldNF $ \ x_a tx -> fmap x_a (lift $$ tx)

a2m :: (Lift g, Applicative (g f)) => APPLICATIVE f :~> g f
a2m = Nat $ foldNAF pure $ \ ryz ty -> ryz <*> (lift $$ ty)

---------------------------------------------------------------------------

joinFunctor :: (Functor f) => (FUNCTOR f :~> f)
joinFunctor = Nat $ foldNF (\ x_a tx -> fmap x_a tx) 

--joinFunctor :: (Functor g) => (f :~> g) -> (FUNCTOR f :~> g)
--joinFunctor o = Nat $ foldNF $ \ x_a tx -> fmap x_a (o $$ tx)

joinApplicative :: (Applicative g) => (f :~> g) -> (APPLICATIVE f :~> g)
joinApplicative o = Nat $ foldNAF pure $ \ ryz ty -> ryz <*> (o $$ ty)

joinMonad :: (Monad g) => (f :~> g) -> (MONAD f :~> g)
joinMonad o = Nat $ foldNM return $ \ tx x_r -> (o $$ tx) >>= x_r


