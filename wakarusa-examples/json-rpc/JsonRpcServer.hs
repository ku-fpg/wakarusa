{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}

-- Experiment with the JsonRpc API

import Prelude hiding ((.), id)
import Control.Category

import Data.Aeson 
import Control.Natural
import Control.Wakarusa.Functor
import Control.Wakarusa.Session
import Control.Wakarusa.JsonRpc

import Control.Wakarusa.Session.Scotty
import Control.Category ((>>>))

import Web.Scotty

import Square

myServer :: JsonRpcSend :~> IO
myServer =  rpcServer            -- parse
        >>> joinMonad evalSquare -- eval 

main = do
  let rule = scottyServer "/rpc" myServer
  scotty 3000 rule
