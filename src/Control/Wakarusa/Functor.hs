{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.Functor where

import Control.Monad.ConstrainedNormal

import Control.Natural

---------------------------------------------------------------------------

class Functor1 h where
 fmap1 :: Natural f g -> Natural (h f) (h g)

---------------------------------------------------------------------------
type FUNCTOR = NF Unconstrained

nf :: Natural m (FUNCTOR m)
nf = Natural liftNF

instance Functor1 (NF c) where
 fmap1 o = Natural $ foldNF (\ x_a tx -> fmap x_a (liftNF (o # tx)))

---------------------------------------------------------------------------
type MONAD = NM Unconstrained

nm :: Natural m (MONAD m)
nm = Natural liftNM

instance Functor1 (NM c) where
  fmap1 o = Natural $ foldNM return (\ tx x_r -> liftNM (o # tx) >>= x_r)


