{-# LANGUAGE PolyKinds, TypeOperators #-}
module Control.Wakarusa.Pointed1 where

import Control.Natural

---------------------------------------------------------------------------
-- | Pointed1 is the Natural Transformation version of Pointed.

class Pointed1 f where 
  point1 :: g :~> f g
