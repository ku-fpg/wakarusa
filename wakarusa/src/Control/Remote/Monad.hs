{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Control.Remote.Monad where

import Control.Monad.IO.Class

-- The Remote Monad has the ability to send a set of commands, f, to a remote object, r,
-- using the capabilites provided by the monad m.
class RemoteMonad m r where
        (#) :: r f -> f a -> m a

instance (MonadIO m, RemoteMonad IO r) => RemoteMonad m r where
       o # f = liftIO (o # f)

-- | The RemoteIO class is the remote monad, specialized to IO.
class RemoteMonad IO r => RemoteIO r

instance RemoteMonad IO r => RemoteIO r


