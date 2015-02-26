{-# LANGUAGE TypeOperators, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
module Control.Wakarusa.Session.Scotty where
        
import Data.Aeson
import Control.Monad.IO.Class  (liftIO)
import Web.Scotty as S

import Control.Natural
import Control.Wakarusa.Session

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

