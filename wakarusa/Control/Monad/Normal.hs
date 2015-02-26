{-# LANGUAGE PolyKinds, RankNTypes, GeneralizedNewtypeDeriving #-}
module Control.Monad.Normal where

import Control.Applicative
import qualified Control.Monad.ConstrainedNormal as N

newtype FUNCTOR f a = FUNCTOR (N.NF N.Unconstrained f a) deriving
  ( Functor )

liftNF :: t a -> FUNCTOR t a
liftNF f = FUNCTOR (N.liftNF f)

foldNF :: (forall x. (x -> a) -> t x -> r) -> FUNCTOR t a -> r
foldNF f (FUNCTOR nf) = N.foldNF f nf

newtype APPLICATIVE f a = APPLICATIVE (N.NAF N.Unconstrained f a) deriving
  ( Functor, Applicative )

liftNAF :: t a -> APPLICATIVE t a
liftNAF f = APPLICATIVE (N.liftNAF f)

foldNAF :: forall a r t. (forall x. x -> r x) -> (forall y z. r (y -> z) -> t y -> r z) -> APPLICATIVE t a -> r a
foldNAF f g (APPLICATIVE naf) = N.foldNAF f g naf

newtype MONAD f a = MONAD (N.NM N.Unconstrained f a) deriving
  ( Functor, Applicative, Monad )

liftNM :: t a -> MONAD t a
liftNM f = MONAD (N.liftNM f)

foldNM :: forall a r t. (a -> r) -> (forall x. t x -> (x -> r) -> r) -> MONAD t a -> r
foldNM f g (MONAD nm) = N.foldNM f g nm

