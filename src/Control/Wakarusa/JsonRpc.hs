{-# LANGUAGE TypeOperators, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
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
import Data.Scientific

import Control.Wakarusa.Session
import Control.Wakarusa.Functor

------------------------------------------------------------------------------------------
-- This is our basic JsonRpc API. It is a reflection of the Session API.
-- Notice how we can send and receive multiple calls, as in the JSON RPC API.

data JsonRpc :: * -> * where
  SendJsonRpc  :: [JsonRpcRequest] -> JsonRpc [JsonRpcResponse]
  CloseJsonRpc ::                  JsonRpc ()

class JsonRpcClass f where
  sendJsonRpc :: [JsonRpcRequest] -> f [JsonRpcResponse]

------------------------------------------------------------------------------------------

data Square :: * -> * where
 Square :: Int -> Square Int      -- (remotely) square a number

class SquareClass f where
  square :: Int -> f Int         

instance SquareClass Square where
  square = Square

instance SquareClass JsonRpcCall where
  square n = JsonRpcCall $ JsonRpcRequest "square" [toJSON n]

-- encoding what Square does
runSquare :: Square :~> JsonRpcCall
runSquare = Nat $ \ f -> case f of
    Square n -> JsonRpcCall $ JsonRpcRequest "square" [toJSON n]

data A :: (* -> *) -> * -> * where
  PureNAF :: a -> A t a
  ApNAF :: A t (y -> z) -> t y -> A t z

runApplicative :: forall f g . (JsonRpcClass g, Monad g) => (APPLICATIVE JsonRpcCall :~> g)
runApplicative  = Nat $ \ f -> do
   let naf = foldNAF PureNAF ApNAF f
   let fn :: JsonRpcClass g => A JsonRpcCall a -> [JsonRpcRequest] -> g ([JsonRpcResponse],a)
       fn (PureNAF a) xs = do res <- sendJsonRpc (reverse xs)
                              return (reverse res,a)
       fn (ApNAF g a) xs = case a of
                             JsonRpcCall call -> do
                              (JsonRpcResponse y : ys,r) <- fn g (call : xs)
                              case fromJSON y of
                                Error {} -> error $ "failed"
                                Success v -> return (ys, r v)
   (_,a) <- fn naf []
   return a


------------------------------------------------------------------------------------------
-- These encode how the JSON RPC uses JSON

data JsonRpcRequest = JsonRpcRequest Text [Value]

instance ToJSON JsonRpcRequest where
   toJSON (JsonRpcRequest method args) = object ["jsonrpc" .= (2.0 :: Double), "method" .= method, "args" .= args, "id" .= Null]

instance FromJSON JsonRpcRequest where
   parseJSON (Object v) = JsonRpcRequest
                      <$> v .: "method"
                      <*> v .: "args"
   parseJSON _          = mzero

data JsonRpcResponse = JsonRpcResponse Value

instance ToJSON JsonRpcResponse where
   toJSON (JsonRpcResponse v) = object ["jsonrpc" .= (2.0 :: Double), "result" .= v, "id" .= Null]

instance FromJSON JsonRpcResponse where
   parseJSON (Object v) = JsonRpcResponse
                      <$> v .: "result"
   parseJSON _          = mzero

------------------------------------------------------------------------------------------

-- It might 
data JsonRpcCall :: * -> * where
  JsonRpcCall :: FromJSON a => JsonRpcRequest -> JsonRpcCall a

------------------------------------------------------------------------------------------
{-
jsonRpcClient :: Natural JsonRpc (FUNCTOR (Session LBS.ByteString))
jsonRpcClient = Natural $ \ f ->
  case f of
    SendJsonRpc  msg -> let fn Nothing = []
                            fn (Just v') = case decode v' of 
                                            Nothing -> []
                                            Just v'' -> v''
                        in fmap fn $ nf # Send (encode msg)
    SendJsonRpc_ msg -> nf # Send_ (encode msg)
    CloseJsonRpc     -> nf # Close

-- OLD API
-- jsonRpcClient :: Monad m => Natural (Session LBS.ByteString) m -> Natural JsonRpc m

jsonRpcServer :: Natural (Session LBS.ByteString) (MONAD JsonRpc)
jsonRpcServer = Natural $ \ f -> case f of
   Send msg -> case decode msg of 
                  Nothing -> fail "jsonRpcServer $ Send _"
                  Just msg' -> fmap (Just . encode) 
                             $ nm # SendJsonRpc msg'
   Send_ msg -> case decode msg of 
                  Nothing -> fail "jsonRpcServer $ Send_ _"
                  Just msg' -> nm # SendJsonRpc_ msg'
   Close -> nm # CloseJsonRpc 

-- OLD API
-- jsonRpcServer :: Monad m => Natural JsonRpc m -> Natural (Session LBS.ByteString) m

------------------------------------------------------------------------------------------

data Square :: * -> * where
 Square :: Int -> Square Int      -- (remotely) square a number

square :: Int -> FUNCTOR Square Int
square n = nf # Square n

class JsonRpcAPI f where
  jsonRpcAPI :: Natural f (FUNCTOR JsonRpc)
--  jsonRpcAPI1 :: Natural JsonRpc ()

instance JsonRpcAPI Square where
  jsonRpcAPI = Natural $ \ f -> case f of
                  Square n -> fmap (\ [JsonRpcResult (Number v)] -> case toBoundedInteger v of
                                                                      Nothing -> error "bounded problem"
                                                                      Just i -> i)
                            $ nf # SendJsonRpc [JsonRpcCall "square" [Number $ fromInteger $ fromIntegral $ n ]]


-- APPLICATIVE Square 
-- JsonRpc
-- Session [JsonRpcCall] [JsonRpcResult]
-- JsonRpc
-- APPLICATIVE Square


-}