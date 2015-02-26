{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}

-- Experiment with the JsonRpc API

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative

import Data.Aeson 
import Control.Natural
import Control.Monad.Normal
import Control.Wakarusa.Session
import Control.Wakarusa.JsonRpc
import Control.Wakarusa.Join1

import Control.Wakarusa.Session.Wreq

import Square

myClient :: MONAD Square :~> MONAD JsonRpcSend
myClient = runMonad


main = do
  let myApp :: MONAD Square :~> IO
      myApp = runMonad >>> run1 (wreqClient "http://localhost:3000/rpc")

  -- Test an outer chain of calls
  r <- myApp $$ square 4
  r <- myApp $$ square r
  print r

  -- Test an inner chain of calls
  r <- myApp # do
          a <- square 4
          square a
  print r

  let myApp :: APPLICATIVE Square :~> IO
      myApp = runApplicative >>> run1 (wreqClient "http://localhost:3000/rpc")

  -- Test an outer chain of calls
  r <- myApp $$ square 4
  r <- myApp $$ square r
  print r

  -- Test an inner set of (parallel) calls
  (r1,r2) <- myApp #
          pure (,) 
            <*> square 4 
            <*> square 9
  print (r1,r2)
