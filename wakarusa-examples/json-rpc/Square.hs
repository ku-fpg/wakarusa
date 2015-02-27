{-# LANGUAGE TypeOperators, KindSignatures, OverloadedStrings, GADTs, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
module Square where

import Data.Aeson 
import Control.Natural  
import Control.Wakarusa.Pointed1
import Control.Wakarusa.Session
import Control.Wakarusa.JsonRpc
        
data Square :: * -> * where
 Square :: Int -> Square Int      -- (remotely) square a number

class Squarer f where
  square :: Int -> f Int         

instance Squarer Square where
  square = Square

instance Pointed1 h => Squarer (h Square) where
  square n = point1 $$ square n

instance JsonRpc Square where
  encodeRpcCall (Square n) = call "square" [toJSON n]
  decodeRpcCall (JsonRpcCallee "square" [v]) = 
                   do v' <- get v
                      r <- square v'
                      return (result r)

evalSquare :: Square :~> IO
evalSquare = Nat $ \ f -> case f of
   Square n -> return (n * n)
