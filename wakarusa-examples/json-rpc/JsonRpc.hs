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
  let session = wreqClient "http://localhost:3000/rpc"
  let myApp :: MONAD Square :~> IO
      myApp = run1 session . runMonad
  r <- myApp $$ square 4
  r <- myApp $$ square r
  print r
  r <- myApp # do
          a <- square 4
          square a
  print r

  let myApp :: APPLICATIVE Square :~> IO
      myApp = run1 session . runApplicative
  r <- myApp $$ square 4
  r <- myApp $$ square r
  print r

  (r1,r2) <- myApp #
          pure (,) 
            <*> square 4 
            <*> square 9
  print (r1,r2)
