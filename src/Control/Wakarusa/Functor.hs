{-# LANGUAGE TypeOperators, GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.Functor where

import Control.Monad.ConstrainedNormal
import Control.Applicative
import Control.Natural

---------------------------------------------------------------------------

class Functor1 h where
 fmap1 :: (f :~> g) -> (h f :~> h g)

---------------------------------------------------------------------------
type FUNCTOR = NF Unconstrained

nf :: m :~> FUNCTOR m
nf = Nat liftNF

instance Functor1 (NF c) where
 fmap1 o = Nat $ foldNF $ \ x_a tx -> fmap x_a (liftNF (o $$ tx))
---------------------------------------------------------------------------
type APPLICATIVE = NAF Unconstrained

naf :: m :~> APPLICATIVE m
naf = Nat liftNAF

instance Functor1 (NAF c) where
 fmap1 o = Nat $ foldNAF pure $ \ ryz ty -> ryz <*> liftNAF (o $$ ty)

---------------------------------------------------------------------------
type MONAD = NM Unconstrained

nm :: m :~> MONAD m
nm = Nat liftNM

instance Functor1 (NM c) where
  fmap1 o = Nat $ foldNM return $ \ tx x_r -> liftNM (o $$ tx) >>= x_r


