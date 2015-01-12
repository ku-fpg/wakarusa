{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, NullaryTypeClasses, FlexibleContexts #-}
module Control.Remote.Object where

-- The is the 'local' handle into remote things.
data Object :: (* -> *) -> * where
  Object :: (forall a . f a -> IO a) -> Object f

-- The invoke method, that used an f-algebra 
(#) :: Object f -> f a -> IO a
Object f # g = f g

