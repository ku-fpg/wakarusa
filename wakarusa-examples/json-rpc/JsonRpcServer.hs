{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}

-- Experiment with the JsonRpc API

import Prelude hiding ((.), id)
import Control.Category

import Data.Aeson 
import Control.Natural
import Control.Wakarusa.Join1
import Control.Wakarusa.Session
import Control.Wakarusa.JsonRpc

import Control.Wakarusa.Session.Scotty
import Control.Category ((>>>))

import Web.Scotty

import Square

main = do
  let rule = scottyServer "/rpc" $ rpcServer >>> run1 evalSquare
  scotty 3000 rule
