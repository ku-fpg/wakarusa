{-# LANGUAGE GADTs, RankNTypes, KindSignatures #-}
module Control.Natural where

import qualified Control.Category as C

-- sometimes called :~>

newtype Natural :: (* -> *) -> (* -> *) -> * where
  Natural :: (forall a . f a -> g a) -> Natural f g

-- The invoke method for the natural transformation.
-- Notice there is no mention of monads or functors here.
--(#) :: Natural f g -> f a -> g a
--Natural f # g = f g

class Transformation t where
   (#) :: t f g -> f a -> g a
   
instance Transformation Natural where
   Natural f # g = f g

-- Free theorem for natural transformations: fmap h . r == r . fmap h, 
-- (From Bartosz Milewski's blog, Functional Pearl: F for Functor, and other places)

newtype List2 a = List2 [[a]]

-- I'm actually surprised this works. The kind
-- Natural, and the kind of cat (from the class)
-- do not match. Some form of higher-kindiness
-- must be going on here.
instance C.Category Natural where
  id = Natural id
  Natural g . Natural h = Natural (g . h)
  
