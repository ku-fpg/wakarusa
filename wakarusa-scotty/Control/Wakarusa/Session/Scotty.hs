{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.Session.Scotty where
        
import Web.Scotty as S
import Data.Aeson

import Control.Natural
import Control.Wakarusa.Session
import Control.Monad.IO.Class  (MonadIO, liftIO)
--import Network.Wreq as W


-- Should these be specific, and just something like Send rather than Sender?

scottyServer :: ( FromJSON req, ToJSON resp,  Sender req resp f )
             => String
             -> (f :~> IO)
             -> ScottyM ()
scottyServer nm session = do
        -- We should think about authentication, etc. This is wide, wide open.
        S.post (literal nm) $ do
                dat <- jsonData
                res <- liftIO $ session $$ send dat
                S.json res

{-
wreqClient :: ( ToJSON req,  FromJSON resp, Sendee req resp f )
           => String
           -> (f :~> IO)
wreqClient nm = Nat $ \ f -> case recv f of
        Send msg -> do 
                r <- W.post nm (toJSON msg)
                return $ undefined
-}