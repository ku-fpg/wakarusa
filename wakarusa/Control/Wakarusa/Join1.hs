{-# LANGUAGE PolyKinds, TypeOperators, MultiParamTypeClasses, FunctionalDependencies, ConstraintKinds #-}
module Control.Wakarusa.Join1 where

import Control.Natural
import Control.Category ((>>>))

import Control.Wakarusa.Functor1
import Control.Wakarusa.Pointed1

---------------------------------------------------------------------------
-- | Join1 is the Natural Transformation version of monadic join

class (Pointed1 h, Functor1 h) => Join1 h c | h -> c where
  join1 :: c g => h g :~> g

-- generic run, inside a context
run1 :: Join1 h c => (c g) => (f :~> g) -> h f :~> g
run1 o = fmap1 o >>> join1

