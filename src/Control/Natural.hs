{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Control.Natural where

import qualified Control.Category as C

-- | A (Natural) Transformation is inside t, and contains the (typically 'Functor's) f and g.
--
-- The order of arguments allows the use of GeneralizedNewtypeDeriving to wrap
-- a 'Natural', but maintain the 'Transformation' constraint. Thus, '#' can
-- be used on abstract types.
class Transformation f g t | t -> f, t -> g where
  -- | The invoke method for a natural transformation.
  -- Notice there is no mention of monads or functors here.
  (#) :: t -> f a -> g a
   
-- Free theorem for natural transformations: fmap h . r == r . fmap h, 
-- (From Bartosz Milewski's blog, Functional Pearl: F for Functor, and other places)
-- Sometimes t is called a functor morphism.

{-# RULES "natural free theorem" [~] 
    forall h (r :: (Functor f, Functor g, Transformation f g t) => t) . 
    fmap h . (r #) = (r #) . fmap h 
  #-}


newtype Natural :: (* -> *) -> (* -> *) -> * where
  Natural :: (forall a . f a -> g a) -> Natural f g

instance Transformation f g (Natural f g) where
   Natural f # g = f g

-- I'm actually surprised this works. The kind
-- of :->, and the kind of cat (from the class)
-- do not match. Some form of higher-kindiness
-- must be going on here.
instance C.Category Natural where
  id = Natural id
  Natural g . Natural h = Natural (g . h)
  
