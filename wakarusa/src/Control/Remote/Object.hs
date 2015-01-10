{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances #-}
module Control.Remote.Object where

import Control.Monad.IO.Class

import Control.Remote.Monad 

-- The is the 'local' handle into remote things.
data Object :: (* -> *) -> * where
  Object :: (forall a m . f a -> IO a) -> Object f

instance MonadIO m => RemoteMonad m Object where
  (#) (Object g) f = liftIO $ g f
