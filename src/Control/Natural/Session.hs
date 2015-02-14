{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Natural.Session where

import Data.ByteString (ByteString)

import Control.Natural (Natural)

-- | The 'Session' is our interface for sending and receiving ByteString messages over a network.
-- Clients build on top of 'Natural' ('Session' 'ByteString') m; Servers build 'Natural' ('Session' 'ByteString') m.

data Session :: * -> * -> * where
  Send  :: msg -> Session msg (Maybe msg) -- Messages that have a reply
  Send_ :: msg -> Session msg ()                 -- Messages that do not need reply
  Close ::        Session msg ()                 -- Last action; can free session resourses.

