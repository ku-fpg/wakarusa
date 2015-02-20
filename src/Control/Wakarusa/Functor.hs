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
  lift  :: g a -> f g a
  lift' :: g :~> f g
  lift' = Nat lift

instance Lift FUNCTOR where
  lift = liftNF

instance Lift APPLICATIVE where
  lift = liftNAF

instance Lift MONAD where
  lift = liftNM

liftNT :: Lift h => (m :~> h m)
liftNT = Nat lift

--instance Lift (f (:~>)) where
---  lift = error "X" :: () -> ()

--- m a -> t m a
--- m a -> t m a

lift'' :: (g :~> f g) -> g a -> f g a
lift'' o = (o $$)

---------------------------------------------------------------------------
type FUNCTOR = NF Unconstrained

instance Functor1 FUNCTOR where
 fmap1 o = Nat $ foldNF $ \ x_a tx -> fmap x_a (lift' $$ o $$ tx)

---------------------------------------------------------------------------
type APPLICATIVE = NAF Unconstrained

instance Functor1 (NAF c) where
 fmap1 o = Nat $ foldNAF pure $ \ ryz ty -> ryz <*> liftNAF (o $$ ty)

---------------------------------------------------------------------------
type MONAD = NM Unconstrained

instance Functor1 (NM c) where
  fmap1 o = Nat $ foldNM return $ \ tx x_r -> liftNM (o $$ tx) >>= x_r

---------------------------------------------------------------------------

f2a :: FUNCTOR f :~> APPLICATIVE f
f2a = Nat $ foldNF $ \ x_a tx -> fmap x_a (liftNAF tx)

a2m :: APPLICATIVE f :~> MONAD f
a2m = Nat $ foldNAF return $ \ ryz ty -> ryz <*> liftNM ty

---------------------------------------------------------------------------

joinFunctor :: (Functor f) => (FUNCTOR f :~> f)
joinFunctor = Nat $ foldNF (\ x_a tx -> fmap x_a tx) 

--joinFunctor :: (Functor g) => (f :~> g) -> (FUNCTOR f :~> g)
--joinFunctor o = Nat $ foldNF $ \ x_a tx -> fmap x_a (o $$ tx)

joinApplicative :: (Applicative g) => (f :~> g) -> (APPLICATIVE f :~> g)
joinApplicative o = Nat $ foldNAF pure $ \ ryz ty -> ryz <*> (o $$ ty)

joinMonad :: (Monad g) => (f :~> g) -> (MONAD f :~> g)
joinMonad o = Nat $ foldNM return $ \ tx x_r -> (o $$ tx) >>= x_r


