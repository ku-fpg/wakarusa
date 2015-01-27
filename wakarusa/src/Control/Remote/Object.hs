{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, NullaryTypeClasses, FlexibleContexts #-}
module Control.Remote.Object where

import qualified Control.Category as C

-- The is the 'local' handle into remote things.
-- Not sure about the order of the arguments.
newtype Object :: (* -> *) -> (* -> *) -> * where
  Object :: (forall a . f a -> m a) -> Object m f

-- The invoke method, that used an f-algebra,
-- and lifts it into the m-algebra space. Notice
-- there is no mention of monads here.
(#) :: Object m f -> f a -> m a
Object f # g = f g

-- I'm actually surprised this works. The kind
-- Object, and the kind of cat (from the class)
-- do not match. Some form of higher-kindiness
-- must be going on here.
instance C.Category Object where
  id = Object id
  Object g . Object h = Object (h . g)

--combine :: Object b c -> Object a b -> Object a c:
--combine (Object g) (Object h) = Object (h . g)
