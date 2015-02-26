{-# LANGUAGE PolyKinds, TypeOperators, GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.Functor where

import Control.Applicative
import Control.Monad.ConstrainedNormal
import Control.Natural

import Control.Wakarusa.Functor1
import Control.Wakarusa.Pointed1
import Control.Wakarusa.Join1


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

instance Join1 FUNCTOR Functor where
  join1 = Nat $ foldNF $ \ x_a tx -> fmap x_a tx

instance Join1 APPLICATIVE Applicative where
  join1 = Nat $ foldNAF pure $ \ ryz ty -> ryz <*> ty

instance Join1 MONAD Monad where
  join1 = Nat $ foldNM return $ \ tx x_r -> tx >>= x_r
    

