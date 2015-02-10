{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Natural.Remote where
        
import Control.Natural
import Data.Aeson as A
import Data.Text(Text)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

import Control.Monad.ConstrainedNormal


{-
instance Remote f where
  remote :: f -> Request f
  
data Request :: * -> * where
  SyncMsg :: () -> Request ()
 
-}
----------------------------------------------------------------------------------------------------
data Square :: * -> * where
 Square :: Int -> Square Int      -- (remotely) square a number

-- {"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}

encodeSquare :: Square a -> JsonRpcRequest
encodeSquare (Square n) = ("square", [Number $ fromIntegral n])

decodeSquare :: forall a . Square a -> JsonReqResponse -> Maybe a
decodeSquare (Square {}) (Number n) = return (4 :: Int)

square :: Int -> NAF Unconstrained Square Int
square = liftNAF . Square

runSquare :: Natural JsonRpc2 IO -> Natural (NAF Unconstrained Square) IO
runSquare jsonRpc = Natural $ \ f -> do
   undefined
{-
   let en = encodeSquare f
   [r] <- jsonRpc # JsonRpc2 [en]
   case (decodeSquare f r) of
     Just a -> return a
-}
-- runSquare runJsonRpc # Square 4
theSquare :: Natural (NAF Unconstrained Square) IO
theSquare = runSquare theJsonRpc2

----------------------------------------------------------------------------------------------------

data JsonRpc2 :: * -> * where
 JsonRpc2 :: [JsonRpcRequest] -> JsonRpc2 [JsonReqResponse]

type JsonRpcRequest = (Text,[Value])
type JsonReqResponse = Value

runJsonRpc2 :: Natural JsonRpc IO -> Natural JsonRpc2 IO
runJsonRpc2 rpc = Natural $ \ f -> do
    case f of
      JsonRpc2 reqs -> sequence 
              [ do v <- rpc # (Send $ Object $ HashMap.fromList 
                              [ ("method",String method)
                              , ("params",Array $ Vector.fromList $ args)
                              ])
                   case v of
                    Just (Object v) -> return $ (v HashMap.! "response")
              | (method,args) <- reqs 
              ]

theJsonRpc2 :: Natural JsonRpc2 IO
theJsonRpc2 = runJsonRpc2 theJsonRpc

----------------------------------------------------------------------------------------------------

-- Layers: <user-facing>,<protocol-specific>,<generic>,ByteStream

data JsonRpc :: * -> * where
 Send :: Value -> JsonRpc (Maybe Value)

theJsonRpc :: Natural JsonRpc IO
theJsonRpc = Natural $ \ f ->
  case f of
    Send (Object o) -> do
            print ("->" :: String,o)
            let o' = HashMap.insert "response" (Number 99) o
            print ("<-" :: String,o')
            return (Just (Object o'))
    
--  runJsonRpc # Send (Number 9)

{-
newtype Remote f = Remote (Natural f IO)
  deriving (Transformation f IO)

{-
data RemoteM :: * -> * where
  SendSync :: ByteString -> RemoteM ByteString
  SendASync :: ByteString -> RemoteM ()
-}
    
class RemoteM m where
  sendSync :: ByteString -> m ByteString
  sendAsync :: ByteString -> m ()

class ConnectionM m where
   send :: ByteString -> m ()
   receive :: IO ByteString

data Event = Received [ByteString] | ConnectionClosed

instance Transformation TransportM IO Transport where
   t # NewEndPoint    = t newEndPoint
   t # CloseTransport = t closeTransport

data TransportM m where
  NewEndPoint     :: m (Either (TransportError NewEndPointErrorCode) EndPoint)
  CloseTransport  :: m ()

class TransportC m where
  newEndPoint :: 

class EndPointM m where
  connect :: ConnectionM m => EndPointAddress -> Reliability -> ConnectHints -> m (Natural f IO)

class ConnectionM m where
  send :: [...] -> m ()

-- Event  
        
newTransport :: TransportM f => Network -> Natural f IO
newTransport t = Natural fn
  where fn :: TransportM 

newEndPoint
-}