{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.Functor where

import Control.Monad.ConstrainedNormal

import Control.Natural

nf :: Natural m (NF Unconstrained m)
nf = Natural liftNF

class Functor1 h where
 nmap :: Natural f g -> Natural (h f) (h g)

instance Functor1 (NF c) where
 nmap o = Natural $ foldNF (\ x_a tx -> fmap x_a (liftNF (o # tx)))


