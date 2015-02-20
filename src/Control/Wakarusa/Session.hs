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

class Sender f msg reply | f -> msg, f -> reply where
  send :: msg -> f reply

instance (Lift h, Sender f msg repl) => Sender (h f) msg repl where
  send msg = lift $$ send msg

data Send :: * -> * -> * -> * where
  Send  :: msg -> Send msg repl repl              -- Messages that have a reply

instance Sender (Send msg repl) msg repl where
  send = Send
