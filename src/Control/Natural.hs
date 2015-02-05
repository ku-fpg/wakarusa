{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Control.Natural where

import qualified Control.Category as C

class Transformation t f g | t -> f, t -> g where
  -- | The invoke method for a natural transformation.
  -- Notice there is no mention of monads or functors here.
  (#) :: t -> f a -> g a
   
-- Free theorem for natural transformations: fmap h . r == r . fmap h, 
-- (From Bartosz Milewski's blog, Functional Pearl: F for Functor, and other places)
-- Sometimes t is called a functor morphism.

{-# RULES "natural free theorem" [~] 
    forall h (r :: (Functor f, Functor g, Transformation t f g) => t) . 
    fmap h . (r #) = (r #) . fmap h 
  #-}

-- Originally called Natural.
newtype Natural :: (* -> *) -> (* -> *) -> * where
  Natural :: (forall a . f a -> g a) -> Natural f g

instance Transformation (Natural f g) f g where
   Natural f # g = f g

-- I'm actually surprised this works. The kind
-- of :->, and the kind of cat (from the class)
-- do not match. Some form of higher-kindiness
-- must be going on here.
instance C.Category Natural where
  id = Natural id
  Natural g . Natural h = Natural (g . h)
  
