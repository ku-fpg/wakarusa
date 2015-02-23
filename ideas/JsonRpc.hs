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
--      . joinMonad network    -- network
      . runMonad             -- eval the embedded method call

myClient :: MONAD Square :~> MONAD JsonRpcSend
myClient = runMonad

myServer :: JsonRpcSend :~> IO
myServer = joinMonad evalSquare -- eval 
         . server               -- parse

-- simulate the connect
networker :: ( ToJSON req, FromJSON req'
             , ToJSON resp', FromJSON resp
             , Sendee req resp f
             , Sender req' resp' f') 
          => (f' :~> IO) -> IO (f :~> IO)
networker o = return $ Nat $ \ f -> case recv f of
  Send msg -> do Success msg' <- return (fromJSON (toJSON msg))
                 rep <- o $$ send msg'
                 Success rep' <- return (fromJSON (toJSON rep))
                 return rep'

main = do
  session <- networker $ myServer
  let myApp :: MONAD Square :~> IO
      myApp = joinMonad session . runMonad
  r <- myApp $$ square 4
  r <- myApp $$ square r
  print r
  r <- myApp # do
          a <- square 4
          square a
  print r

