{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Wakarusa.JsonRpc where
        
import Prelude hiding ((.))
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
import Control.Category((.))

import Control.Wakarusa.Session
import Control.Wakarusa.Functor
import Control.Wakarusa.Pointed1


------------------------------------------------------------------------------------------
-- All sendable method calls are instances of 'JsonRpc'
class JsonRpc f where
  -- These are both natural transformations
  encodeRpcCall :: f a  -> JsonRpcCall a       -- always works
  decodeRpcCall :: Send JsonRpcRequest JsonRpcResponse a -> MONAD f a           -- can fail


------------------------------------------------------------------------------------------
-- Small DSL for building JsonRpc instances

call nm args = JsonRpcCall $ JsonRpcRequest nm args

get a = do Success v <- return (fromJSON a)
           return v
           
result :: (ToJSON a) => a -> JsonRpcResponse
result = JsonRpcResponse . toJSON 

----------------------------------------------------------------------------------------
-- | JsonRpcSend is a specialized verasion of 'Send'
newtype JsonRpcSend a = JsonRpcSend (Send [JsonRpcRequest] [JsonRpcResponse] a)
        deriving ( Sender [JsonRpcRequest] [JsonRpcResponse]
                 , Sendee [JsonRpcRequest] [JsonRpcResponse]
                 )

------------------------------------------------------------------------------------------
-- running the applicative
                      
data A :: (* -> *) -> * -> * where
  PureNAF :: a -> A t a
  ApNAF :: A t (y -> z) -> t y -> A t z

-- The simple one; no structure around f.
runId :: forall f g . (JsonRpc f) => (f :~> MONAD JsonRpcSend)
runId  = runApplicative . f2a . point1

runFunctor :: forall f g . (JsonRpc f) => (FUNCTOR f :~> MONAD JsonRpcSend)
runFunctor  = runApplicative . f2a

runApplicative :: forall f g . (JsonRpc f) => (APPLICATIVE f :~> MONAD JsonRpcSend)
runApplicative  = Nat $ \ f -> do 
   let naf = foldNAF PureNAF ApNAF f
   let fn :: A f a -> [JsonRpcRequest] -> MONAD JsonRpcSend ([JsonRpcResponse],a)
       fn (PureNAF a) xs = do res <- send (reverse xs)
                              return (reverse res,a)
       fn (ApNAF g a) xs = case encodeRpcCall a of
                             JsonRpcCall call -> do
                              (JsonRpcResponse y : ys,r) <- fn g (call : xs)
                              case fromJSON y of
                                Error {} -> error $ "failed"
                                Success v -> return (ys, r v)

   (_,a) <- fn naf []
   return a

runMonad :: forall f g . (JsonRpc f) => (MONAD f :~> MONAD JsonRpcSend)
runMonad = Nat $ \ f -> foldNM return bind f
   where bind :: forall x x1 . f x1 -> (x1 -> MONAD JsonRpcSend x) -> MONAD JsonRpcSend x
         bind m k = do r <- runFunctor $$ liftNF m
                       k r

------------------------------------------------------------------------------------------
-- These encode how the JSON RPC uses JSON

data JsonRpcRequest = JsonRpcRequest Text [Value]
        deriving Show
        
instance ToJSON JsonRpcRequest where
   toJSON (JsonRpcRequest method args) = object ["jsonrpc" .= (2.0 :: Double), "method" .= method, "args" .= args, "id" .= Null]

instance FromJSON JsonRpcRequest where
   parseJSON (Object v) = JsonRpcRequest
                      <$> v .: "method"
                      <*> v .: "args"
   parseJSON _          = mzero

data JsonRpcResponse = JsonRpcResponse Value
        deriving Show

instance ToJSON JsonRpcResponse where
   toJSON (JsonRpcResponse v) = object ["jsonrpc" .= (2.0 :: Double), "result" .= v, "id" .= Null]

instance FromJSON JsonRpcResponse where
   parseJSON (Object v) = JsonRpcResponse
                      <$> v .: "result"
   parseJSON _          = mzero

------------------------------------------------------------------------------------------
-- This is the generic version of a JSON RPC Call: fmap (...) (...)

data JsonRpcCall :: * -> * where
  JsonRpcCall :: FromJSON a => JsonRpcRequest -> JsonRpcCall a

------------------------------------------------------------------------------------------

rpcServer :: (JsonRpc f) => JsonRpcSend :~> MONAD f
rpcServer = Nat $ \ f -> case f of
   JsonRpcSend (Send msgs) -> 
                sequence [ decodeRpcCall (Send msg)
                         | msg <- msgs
                         ]

