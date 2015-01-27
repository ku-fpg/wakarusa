{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances #-}
module Control.Remote.Object.MVar where

import Control.Concurrent.MVar

import Control.Remote.Object 

type RemoteMVar s = Object IO (MVarM s)

data MVarM :: * -> * -> * where
  TakeMVar ::           MVarM s s        
  PutMVar  :: s ->      MVarM s ()

runMVarM :: MVar s -> MVarM s a -> IO a
runMVarM var (TakeMVar)  = takeMVar var
runMVarM var (PutMVar s) = putMVar var s

newRemoteMVar :: a -> IO (RemoteMVar a)
newRemoteMVar = new . newMVar

newRemoteEmptyMVar :: IO (RemoteMVar a)
newRemoteEmptyMVar = new newEmptyMVar

new :: IO (MVar a) -> IO (RemoteMVar a)
new f = do
        ref <- f
        return $ Object $ runMVarM ref
 