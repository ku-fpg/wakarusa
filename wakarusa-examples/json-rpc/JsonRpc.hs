{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}

-- Experiment with the JsonRpc API

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative

import Data.Aeson 
import Control.Natural
import Control.Wakarusa.Functor
import Control.Wakarusa.Session
import Control.Wakarusa.JsonRpc

import Control.Wakarusa.Wreq

import Square

myClient :: MONAD Square :~> MONAD JsonRpcSend
myClient = runMonad

{-
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
-}

main = do
  let session = wreqClient "http://localhost:3000/rpc" <<< foo1'
  let myApp :: MONAD Square :~> IO
      myApp = joinMonad session . runMonad
  r <- myApp $$ square 4
  r <- myApp $$ square r
  print r
  r <- myApp # do
          a <- square 4
          square a
  print r

  let myApp :: APPLICATIVE Square :~> IO
      myApp = joinMonad session . runApplicative
  r <- myApp $$ square 4
  r <- myApp $$ square r
  print r

  (r1,r2) <- myApp #
          pure (,) 
            <*> square 4 
            <*> square 9
  print (r1,r2)
