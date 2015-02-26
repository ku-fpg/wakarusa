{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.Wreq where
        
import Data.Aeson

import Control.Natural
import Control.Wakarusa.Session
--import Control.Monad.IO.Class  (MonadIO, liftIO)
import Network.Wreq as W
import Control.Lens ((^?))

wreqClient :: forall req resp . ( ToJSON req,  FromJSON resp )
           => String
           -> (Send req resp :~> IO)
wreqClient nm = Nat $ \ f -> case f of
        Send msg -> do 
                r <- W.post nm (toJSON msg)
                case r ^? responseBody of
                   Nothing -> error "failure" :: IO resp
                   Just bs -> case decode' bs of
                                        Nothing -> error "failed decode"
                                        Just r -> return r
