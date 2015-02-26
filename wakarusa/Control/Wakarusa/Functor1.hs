{-# LANGUAGE PolyKinds, TypeOperators #-}
module Control.Wakarusa.Functor1 where

import Control.Natural

---------------------------------------------------------------------------
-- | `Functor1` is the Natural Transformation version of Functor. Techically, it *is* a functor.

class Functor1 h where
 fmap1 :: (f :~> g) -> (h f :~> h g)

