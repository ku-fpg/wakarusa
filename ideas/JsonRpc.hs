{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}

-- Experiment with the JsonRpc API

import Prelude hiding ((.), id)
import Control.Category

import Control.Natural
import Control.Wakarusa.Functor
import Control.Wakarusa.Session
import Control.Wakarusa.JsonRpc

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
  


