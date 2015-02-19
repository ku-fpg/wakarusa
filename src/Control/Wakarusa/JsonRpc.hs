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
  SendJsonRpc  :: [JsonRpcCall] -> JsonRpc [JsonRpcResult]
  CloseJsonRpc ::                  JsonRpc ()

class JsonRpcClass f where
  sendJsonRpc :: [JsonRpcCall] -> f [JsonRpcResult]

------------------------------------------------------------------------------------------

data Square :: * -> * where
 Square :: Int -> Square Int      -- (remotely) square a number

class SquareClass f where
  square :: Int -> f Int         

data SingleCall :: * -> * where
  SingleCall :: FromJSON a => JsonRpcCall -> SingleCall a

-- encoding what Square does
runSquare :: Square :~> SingleCall
runSquare = Nat $ \ f -> case f of
    Square n -> SingleCall $ JsonRpcCall "square" [toJSON n]

data A :: (* -> *) -> * -> * where
  PureNAF :: a -> A t a
  ApNAF :: A t (y -> z) -> t y -> A t z

runApplicative :: forall f g . (JsonRpcClass g, Monad g) => (f :~> SingleCall) -> (APPLICATIVE f :~> g)
runApplicative o = Nat $ \ f -> do
   let naf = foldNAF PureNAF ApNAF f
   let fn :: JsonRpcClass g => A f a -> [JsonRpcCall] -> g ([JsonRpcResult],a)
       fn (PureNAF a) xs = do res <- sendJsonRpc (reverse xs)
                              return (reverse res,a)
       fn (ApNAF g a) xs = case o $$ a of
                             SingleCall call -> do
                              (JsonRpcResult y : ys,r) <- fn g (call : xs)
                              case fromJSON y of
                                Error {} -> error $ "failed"
                                Success v -> return (ys, r v)
   (_,a) <- fn naf []
   return a

fromJSON' :: SingleCall a -> Value -> Maybe a
fromJSON' (SingleCall {}) v = case fromJSON v of 
                  Error {} -> Nothing
                  Success a -> Just a
                
-- SingleCall :: FromJSON a => JsonRpcCall -> (JsonRpcResult -> a) -> SingleCall a


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