{-# LANGUAGE GADTs, RankNTypes, KindSignatures, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Control.Natural.Remote where
        
import Control.Natural
import Data.Aeson as A

{-
instance Remote f where
  remote :: f -> Request f
  
data Request :: * -> * where
  SyncMsg :: () -> Request ()

data Square :: * -> * where
 Square :: Int -> Square Int      -- (remotely) square a number
 
-}

data JsonRpc :: * -> * where
 Send :: Value -> JsonRpc (Maybe Value)

runJsonRpc :: Natural JsonRpc IO
runJsonRpc = Natural $ \ f ->
  case f of
    Send v -> do
            print ("->",v)
            let v' = v
            print ("<-",v')
            return (Just v')
    

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