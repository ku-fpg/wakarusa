{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.Wreq where
        
import Data.Aeson

import Control.Natural
import Control.Wakarusa.Session
--import Control.Monad.IO.Class  (MonadIO, liftIO)
import Network.Wreq as W

wreqClient :: ( ToJSON req,  FromJSON resp, Sendee req resp f )
           => String
           -> (f :~> IO)
wreqClient nm = Nat $ \ f -> case recv f of
        Send msg -> do 
                r <- W.post nm (toJSON msg)
                return $ undefined
