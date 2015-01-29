{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances #-}
module Control.Natural.MVar where

import Control.Concurrent.MVar

import Control.Natural

type RemoteMVar s = Natural (MVarM s) IO

data MVarM :: * -> * -> * where
  TakeMVar ::           MVarM s s        
  PutMVar  :: s ->      MVarM s ()

runMVarM :: MVar s -> MVarM s a -> IO a
runMVarM var (TakeMVar)  = takeMVar var
runMVarM var (PutMVar s) = putMVar var s

new :: IO (MVar a) -> IO (RemoteMVar a)
new f = do
        ref <- f
        return $ Natural $ runMVarM ref
