{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Control.Remote.Monad where

import Control.Monad.IO.Class

import qualified Control.Remote.Object as O

-- The Remote Monad has the ability to send a set of commands, f, to a remote object, r,
-- using the capabilites provided by the monad m. This is a generalization of the (#)
-- function provided in Control.Remote.Object.
class RemoteMonad m r where
        (#) :: r f -> f a -> m a

-- not sure about this
instance (MonadIO m, RemoteMonad IO r) => RemoteMonad m r where
       o # f = liftIO (o # f)

instance RemoteMonad IO O.Object where
       (#) = (O.#)

