{-# LANGUAGE UndecidableInstances, FunctionalDependencies, TypeOperators, GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.Session where

import Data.ByteString (ByteString)
import Control.Applicative
import Control.Monad.ConstrainedNormal

import Control.Natural

import Control.Wakarusa.Functor

--import Control.Natural (Natural)
---------------------------------------------------------------------------------

class Closer f where
  -- | It is polite to tell a session that you are closing
  close :: f ()
  
instance (Lift h, Closer f) => Closer (h f) where
  close = lift $$ close

data Close :: * -> * where
  Close :: Close ()

instance Closer Close where
  close = Close

---------------------------------------------------------------------------------

class Sender msg reply f | f -> msg, f -> reply where
  send :: msg -> f reply

instance (Lift h, Sender msg repl f) => Sender msg repl (h f) where
  send msg = lift $$ send msg

data Send :: * -> * -> * -> * where
  Send  :: msg -> Send msg repl repl -- Messages that have a reply; 
                                     -- implies we've tied msg and repl together (somehow)
instance Sender msg repl (Send msg repl) where
  send = Send

--recv_ :: (Send [JsonRpcRequest] [JsonRpcResponse] t1 -> t4 t1) -> JsonRpcSend t1 -> t4 t1

class Sendee msg reply f | f -> msg, f -> reply where
  recv :: f a -> Send msg reply a

instance Sendee msg repl (Send msg repl) where
  recv msg = msg
  
--data Recv :: * -> * -> * -> * where
--  Recv ::         

