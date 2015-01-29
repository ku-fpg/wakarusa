{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeOperators #-}
module Control.Natural where

import Control.Transformation
import qualified Control.Category as C

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
  
