{-# LANGUAGE DeriveDataTypeable, TypeOperators, PolyKinds, GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Control.Natural where

-- This module takes the Nat newtype, and some other constructions 

import Data.Data as Data
import Data.Monoid
import Prelude
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
   
infixr 0 #      -- same as ($)

-- Free theorem for natural transformations: fmap h . r == r . fmap h, 
-- (From Bartosz Milewski's blog, Functional Pearl: F for Functor, and other places)
-- Sometimes t is called a functor morphism.

{-# RULES "natural free theorem" [~] 
    forall h (r :: (Functor f, Functor g, Transformation f g t) => t) . 
    fmap h . (r #) = (r #) . fmap h 
  #-}

infixr 0 ~>
-- | A natural transformation from @f@ to @g@
type f ~> g = forall x. f x -> g x

infixr 0 :~>, $$
-- | A natural transformation suitable for storing in a container.
newtype f :~> g = Nat { ($$) :: f ~> g } deriving Typeable

instance f ~ g => Monoid (f :~> g) where
  mempty = Nat id
  mappend (Nat f) (Nat g) = Nat (f . g)

instance Transformation f g (f :~> g) where
   Nat f # g = f g

instance C.Category (:~>) where
  id = Nat id
  Nat g . Nat h = Nat (g . h)
  
