{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}

-- Experiment with the JsonRpc API

import Prelude hiding ((.), id)
import Control.Category

import Data.Aeson 
import Control.Natural
import Control.Wakarusa.Functor
import Control.Wakarusa.Session
import Control.Wakarusa.JsonRpc


data Square :: * -> * where
 Square :: Int -> Square Int      -- (remotely) square a number

class Squarer f where
  square :: Int -> f Int         

instance Squarer Square where
  square = Square

instance Lift h => Squarer (h Square) where
  square n = lift $$ square n

instance JsonRpc Square where
  encodeRpcCall (Square n) = call "square" [toJSON n]
  decodeRpcCall (Send (JsonRpcRequest "square" [v])) = 
                   do v' <- get v
                      r <- square v'
                      return (result r)

evalSquare :: Square :~> IO
evalSquare = Nat $ \ f -> case f of
   Square n -> return (n * n)

type Id x = x :~> x

myApp :: MONAD Square :~> IO
myApp = joinMonad evalSquare -- eval 
      . joinMonad server     -- parse
      . id                   -- network
      . runMonad             -- eval the embedded method call

main = do
  r <- myApp $$ square 4
  r <- myApp $$ square r
  print r
  r <- myApp # do
          a <- square 4
          square a
  print r

