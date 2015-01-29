{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Control.Transformation where


class Transformation t f g | t -> f, t -> g where
  -- | The invoke method for a natural transformation.
  -- Notice there is no mention of monads or functors here.
  (#) :: t -> f a -> g a
   
-- Free theorem for natural transformations: fmap h . r == r . fmap h, 
-- (From Bartosz Milewski's blog, Functional Pearl: F for Functor, and other places)

{-# RULES "natural free theorem" [~] 
    forall h (r :: (Functor f, Functor g, Transformation t f g) => t) . 
    fmap h . (r #) = (r #) . fmap h 
  #-}

