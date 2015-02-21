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


myApp :: Square :~> IO
myApp = joinMonad evalSquare -- eval 
      . joinMonad server     -- parse
      . id                   -- network
      . runId                -- eval the embedded method call

main = do
  r <- myApp $$ Square 4
  print r
  


