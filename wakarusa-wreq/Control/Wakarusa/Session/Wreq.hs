{-# LANGUAGE TypeOperators, OverloadedStrings, MultiParamTypeClasses, GADTs #-}
module Control.Wakarusa.Session.Wreq where
        
import Data.Aeson

import Control.Natural
import Control.Transformation
import Control.Wakarusa.Session
import Network.Wreq as W
import Control.Lens ((^?))

wreqClient :: ( ToJSON req,  FromJSON resp, Sendee req resp f )
           => String
           -> (f :~> IO)
wreqClient nm = Nat $ \ f -> case recv f of
        Send msg -> do 
                r <- W.post nm (toJSON msg)
                case r ^? responseBody of
                   Nothing -> error "failure" 
                   Just bs -> case decode' bs of
                                        Nothing -> error "failed decode"
                                        Just r -> return r
