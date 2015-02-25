{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Natural.MVar where

import Control.Concurrent.MVar

import Control.Natural

-- | This is more an example of how to build a Natural API.
-- We still need to deside of we abstact over IO, or MonadIO m.

newtype NatualMVar s = NatualMVar (Natural (MVarM s) IO)
  deriving (Transformation (MVarM s) IO)

data MVarM :: * -> * -> * where
  TakeMVar ::           MVarM s s        
  PutMVar  :: s ->      MVarM s ()

runMVarM :: MVar s -> MVarM s a -> IO a
runMVarM var (TakeMVar)  = takeMVar var
runMVarM var (PutMVar s) = putMVar var s

new :: IO (MVar a) -> IO (RemoteMVar a)
new f = do
        ref <- f
        return $ RemoteMVar $ Natural $ NatualMVar ref
