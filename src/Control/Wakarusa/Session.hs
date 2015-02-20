{-# LANGUAGE TypeOperators, GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.Session where

import Data.ByteString (ByteString)
import Control.Applicative
import Control.Monad.ConstrainedNormal

import Control.Natural

import Control.Wakarusa.Functor

--import Control.Natural (Natural)

class Close f where
  -- | It is polite to tell a session that you are closing
  close :: f ()
  
instance (Lift h, Close f) => Close (h f) where
  close = lift $$ close

class AsyncSend f where
  asyncSend :: msg -> f msg ()

class SyncSend f where
  syncSend :: msg -> f msg reply (Maybe reply)

data SyncSendD :: * -> * -> * -> * where
  SyncSend :: msg -> SyncSendD msg reply (Maybe reply)

instance SyncSend SyncSendD where
  syncSend = SyncSend

-- | The 'Session' is our interface for sending and receiving ByteString messages over a network.
-- Clients build on top of 'Natural' ('Session' 'ByteString') m; Servers build 'Natural' ('Session' 'ByteString') m.

data Session :: * -> * -> * -> * where
  Send  :: msg -> Session msg repl repl              -- Messages that have a reply
  Close ::        Session msg repl ()                -- Last action; can free session resourses.
        