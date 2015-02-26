{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}

-- Experiment with the JsonRpc API

import Prelude hiding ((.), id)
import Control.Category

import Data.Aeson 
import Control.Natural
import Control.Wakarusa.Functor
import Control.Wakarusa.Session
import Control.Wakarusa.JsonRpc

import Control.Wakarusa.Scotty

import Web.Scotty

import Square

type Id x = x :~> x

myApp :: MONAD Square :~> IO
myApp = joinMonad evalSquare -- eval 
      . joinMonad server     -- parse
--      . joinMonad network    -- network
      . runMonad             -- eval the embedded method call

myClient :: MONAD Square :~> MONAD JsonRpcSend
myClient = runMonad

myServer :: JsonRpcSend :~> IO
myServer = joinMonad evalSquare -- eval 
         . server               -- parse

-- simulate the connect
networker :: ( ToJSON   req,  FromJSON resp, Sendee req  resp  f
             , FromJSON req', ToJSON resp',  Sender req' resp' f'
             ) 
          => (f' :~> IO) -> IO (f :~> IO)       
networker o = return $ Nat $ \ f -> case recv f of
  Send msg -> do Success msg' <- return (fromJSON (toJSON msg))
                 rep <- o $$ send msg'
                 Success rep' <- return (fromJSON (toJSON rep))
                 return rep'

main = do
  let s :: Send [JsonRpcRequest] [JsonRpcResponse] :~> IO
      s = Nat $ \ f -> case f of
               Send msg -> do
                 print msg
                 return []
  let rule = scottyServer "/rpc" s
  scotty 3000 rule
