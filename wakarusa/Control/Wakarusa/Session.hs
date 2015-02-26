{-# LANGUAGE UndecidableInstances, FunctionalDependencies, TypeOperators, GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.Session where

import Data.ByteString (ByteString)
import Control.Applicative
import Control.Monad.ConstrainedNormal

import Control.Natural

import Control.Wakarusa.Functor
import Control.Wakarusa.Functor1
import Control.Wakarusa.Pointed1

--import Control.Natural (Natural)
---------------------------------------------------------------------------------

class Closer f where
  -- | It is polite to tell a session that you are closing
  close :: f ()
  
instance (Pointed1 h, Closer f) => Closer (h f) where
  close = point1 $$ close

data Close :: * -> * where
  Close :: Close ()

instance Closer Close where
  close = Close

---------------------------------------------------------------------------------

-- | Send-ing a request, with a reply
data Send :: * -> * -> * -> * where
  Send  :: msg -> Send msg repl repl -- Messages that have a reply; 
                                     -- implies we've tied msg and repl together (somehow)

-- | A Sender can send a message. (A Sender is a way of constructing a 'Send')
class Sender msg reply f | f -> msg, f -> reply where
  send :: msg -> f reply

instance (Pointed1 h, Sender msg repl f) => Sender msg repl (h f) where
  send msg = point1 $$ send msg

instance Sender msg repl (Send msg repl) where
  send = Send
  
-- | A Sendee can be sent a message. (A Sendee is a way of deconstructing to a 'Send'.)
class Sendee msg reply f | f -> msg, f -> reply where
  recv :: f a -> Send msg reply a

instance Sendee msg repl (Send msg repl) where
  recv msg = msg

