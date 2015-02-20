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


------------------------------------------------------------------------------------------

class JsonRpc f where
  -- These are both natural transformations
  encodeRpcCall :: f a                                   -> JsonRpcCall a       -- always works
  encodeRpcCall' :: f a  -> Send JsonRpcRequest JsonRpcResponse a       -- always works

  decodeRpcCall :: Send JsonRpcRequest JsonRpcResponse a -> MONAD f a           -- can fail

-- decodeRpcCall . encodeRpcCall  == liftNM

------------------------------------------------------------------------------------------
-- This is our basic JsonRpc API. It is a reflection of the Session API.
-- Notice how we can send and receive multiple calls, as in the JSON RPC API.
{-
data JsonRpc :: * -> * where
  SendJsonRpc  :: [JsonRpcRequest] -> JsonRpc [JsonRpcResponse]
  CloseJsonRpc ::                     JsonRpc ()

class Close f => JsonRpcClass f where
  sendJsonRpc :: [JsonRpcRequest] -> f [JsonRpcResponse]

instance (JsonRpcClass f, Lift h) => JsonRpcClass (h f) where
  sendJsonRpc n = lift $$ sendJsonRpc n
-}

class (Monad f, Sender f [JsonRpcRequest] [JsonRpcResponse]) => JsonRpcr f

class (Monad f, Sender f JsonRpcRequest JsonRpcResponse) => JsonSingleRpcr f

------------------------------------------------------------------------------------------
-- Example defintion

data Square :: * -> * where
 Square :: Int -> Square Int      -- (remotely) square a number

class Squarer f where
  square :: Int -> f Int         

instance Squarer Square where
  square = Square

instance Lift h => Squarer (h Square) where
  square n = lift $$ square n

instance JsonRpc Square where
  encodeRpcCall (Square n) = JsonRpcCall $ JsonRpcRequest "square" [toJSON n]
  decodeRpcCall (Send (JsonRpcRequest "square" [v])) = 
                   do Success v' <- return (fromJSON v)
                      r <- square v'
                      return $ JsonRpcResponse $ toJSON $ r

------------------------------------------------------------------------------------------
-- running the applicative
                      
data A :: (* -> *) -> * -> * where
  PureNAF :: a -> A t a
  ApNAF :: A t (y -> z) -> t y -> A t z

runFunctor :: forall f g . (JsonRpc f, JsonRpcr g) => (FUNCTOR f :~> g)
runFunctor  = runApplicative . f2a

runApplicative :: forall f g . (JsonRpc f, JsonRpcr g) => (APPLICATIVE f :~> g)
runApplicative  = Nat $ \ f -> do 
   let naf = foldNAF PureNAF ApNAF f
   let fn :: JsonRpcr g => A f a -> [JsonRpcRequest] -> g ([JsonRpcResponse],a)
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

runMonad :: forall f g . (JsonRpc f, JsonRpcr g) => (MONAD f :~> g)
runMonad = Nat $ \ f -> foldNM return bind f
   where bind :: forall x x1 . JsonRpcr g => f x1 -> (x1 -> g x) -> g x
         bind m k = do r <- runFunctor $$ liftNF m
                       k r

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

-- This is the generic version of a JSON RPC Call.

data JsonRpcCall :: * -> * where
  JsonRpcCall :: (ToJSON a, FromJSON a) => JsonRpcRequest -> JsonRpcCall a
  JsonRpcClose :: JsonRpcCall ()

------------------------------------------------------------------------------------------
{-
        

class JsonRpcMatch f where
  rpcMatch :: JsonRpcCall a -> MONAD f a

instance JsonRpcMatch Square where
   rpcMatch (JsonRpcCall (JsonRpcRequest "square" [v])) = 
                   do Success v' <- return (fromJSON v)
                      r <- square v'
                      Success r <- return (fromJSON (toJSON v))
                      return r

------------------------------------------------------------------------------------------
-}

server :: (JsonRpc f) => (Send [JsonRpcRequest] [JsonRpcResponse]) :~> MONAD f
server = Nat $ \ f -> case f of
   Send msgs -> sequence [ decodeRpcCall (Send msg)
                         | msg <- msgs
                         ]

{-
{-
splitRpcCall :: JsonRpc :~> MONAD JsonRpcCall
splitRpcCall = Nat fn
  where fn :: JsonRpc a -> MONAD JsonRpcCall a
        fn (SendJsonRpc calls) = do rep <- 
--dispatchSquare = Nat $ \ f -> case f of
--   SendJsonRpc msg -> undefined -- $ \ f -> case f of {}
-}

match :: JsonRpcCall :~> MONAD Square
match = Nat rpcMatch
   
-- This will be an overloading
dispatchSquare :: Square :~> IO
dispatchSquare = Nat fn
  where
    fn :: Square a -> IO a 
    fn (Square n) = return (n * n)


--            do print $ "calling square (" ++ show n ")"
                

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
-}