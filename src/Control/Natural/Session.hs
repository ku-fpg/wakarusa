{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Natural.Session where

import Data.ByteString (ByteString)

import Control.Natural (Natural)

-- | The 'Session' is our interface for sending and receiving ByteString messages over a network.
-- Clients build on top of 'Natural' 'Session' m; Servers build 'Natural' 'Session' m.

data Session :: * -> * where
  Send  :: ByteString -> Session (Maybe ByteString) -- Messages that have a reply
  Send_ :: ByteString -> Session ()                 -- Messages that do not need reply
  Close ::               Session ()                 -- Last action; can free session resourses.

