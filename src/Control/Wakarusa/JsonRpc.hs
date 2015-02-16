{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.JsonRpc where
        
import Control.Natural
import Data.Aeson as A
import Data.Text(Text)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

import Control.Monad.ConstrainedNormal
import Control.Applicative
import Control.Monad (mzero)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

import Control.Wakarusa.Session
import Control.Wakarusa.Functor

------------------------------------------------------------------------------------------

-- This is our basic JsonRpc API. It is a reflection of the Session API.
-- Notice how we can send and receive multiple calls, as in the JSON RPC API.

data JsonRpc :: * -> * where
  SendJsonRpc  :: [JsonRpcCall] -> JsonRpc [JsonRpcResult]
  SendJsonRpc_ :: [JsonRpcCall] -> JsonRpc ()
  CloseJsonRpc ::                  JsonRpc ()

------------------------------------------------------------------------------------------

-- These encode how the JSON RPC uses JSON

data JsonRpcCall = JsonRpcCall Text [Value]

instance ToJSON JsonRpcCall where
   toJSON (JsonRpcCall method args) = object ["jsonrpc" .= (2.0 :: Double), "method" .= method, "args" .= args, "id" .= Null]

instance FromJSON JsonRpcCall where
   parseJSON (Object v) = JsonRpcCall
                      <$> v .: "method"
                      <*> v .: "args"
   parseJSON _          = mzero

data JsonRpcResult = JsonRpcResult Value

instance ToJSON JsonRpcResult where
   toJSON (JsonRpcResult v) = object ["jsonrpc" .= (2.0 :: Double), "result" .= v, "id" .= Null]

instance FromJSON JsonRpcResult where
   parseJSON (Object v) = JsonRpcResult
                      <$> v .: "result"
   parseJSON _          = mzero

------------------------------------------------------------------------------------------

jsonRpcClient' :: Natural JsonRpc (FUNCTOR (Session LBS.ByteString))
jsonRpcClient' = Natural $ \ f ->
  case f of
    SendJsonRpc  msg -> let fn Nothing = []
                            fn (Just v) = case decode v' of 
                                            Nothing -> return []
                                            Just v'' -> return v''
                        in nf # fmap fn $ Send (encode msg)
    SendJsonRpc_ msg -> nf # Send_ (encode msg)
    CloseJsonRpc     -> nf # Close

jsonRpcClient :: Monad m => Natural (Session LBS.ByteString) m -> Natural JsonRpc m
jsonRpcClient g = Natural $ \ f ->
  case f of
    SendJsonRpc  msg -> do v <- g # Send (encode msg)
                           case v of
                             Nothing -> return []
                             Just v' -> case decode v' of 
                                          Nothing -> return []
                                          Just v'' -> return v''
    SendJsonRpc_ msg -> g # Send_ (encode msg)
    CloseJsonRpc     -> g # Close

jsonRpcServer :: Monad m => Natural JsonRpc m -> Natural (Session LBS.ByteString) m
jsonRpcServer g = Natural $ \ f -> case f of
   Send msg -> case decode msg of 
                  Nothing -> fail "jsonRpcServer $ Send _"
                  Just msg' -> do r <- g # SendJsonRpc_ msg'
                                  return $ Just $ encode r
   Send_ msg -> case decode msg of 
                  Nothing -> fail "jsonRpcServer $ Send_ _"
                  Just msg' -> g # SendJsonRpc_ msg'
   Close -> g # CloseJsonRpc 


-- Can you turn a Natural into a Natural transformer, and factor out the requrement 
-- that the API be a monad?
-- :: Natural f g -> Natural f m -> Natural g m
