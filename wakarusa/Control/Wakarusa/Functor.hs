{-# LANGUAGE PolyKinds, TypeOperators, GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.Functor where

import Control.Applicative
import Control.Monad.ConstrainedNormal
import Control.Natural

import Control.Wakarusa.Functor1
import Control.Wakarusa.Pointed1


instance Pointed1 FUNCTOR where
  point1 = Nat liftNF

instance Pointed1 APPLICATIVE where
  point1 = Nat liftNAF

instance Pointed1 MONAD where
  point1 = Nat liftNM

---------------------------------------------------------------------------
type FUNCTOR = NF Unconstrained

instance Functor1 FUNCTOR where
 fmap1 o = Nat $ foldNF $ \ x_a tx -> fmap x_a (point1 $$ o $$ tx)

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

f2a :: (Pointed1 g, Functor (g f)) => FUNCTOR f :~> g f
f2a = Nat $ foldNF $ \ x_a tx -> fmap x_a (point1 $$ tx)

a2m :: (Pointed1 g, Applicative (g f)) => APPLICATIVE f :~> g f
a2m = Nat $ foldNAF pure $ \ ryz ty -> ryz <*> (point1 $$ ty)

---------------------------------------------------------------------------

joinFunctor :: (Functor f) => (FUNCTOR f :~> f)
joinFunctor = Nat $ foldNF (\ x_a tx -> fmap x_a tx) 

--joinFunctor :: (Functor g) => (f :~> g) -> (FUNCTOR f :~> g)
--joinFunctor o = Nat $ foldNF $ \ x_a tx -> fmap x_a (o $$ tx)

joinApplicative :: (Applicative g) => (f :~> g) -> (APPLICATIVE f :~> g)
joinApplicative o = Nat $ foldNAF pure $ \ ryz ty -> ryz <*> (o $$ ty)

joinMonad :: (Monad g) => (f :~> g) -> (MONAD f :~> g)
joinMonad o = Nat $ foldNM return $ \ tx x_r -> (o $$ tx) >>= x_r


