{-# LANGUAGE RankNTypes, GADTs, KindSignatures, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, FunctionalDependencies #-}

import Data.IORef

class Remote r where
        (#) :: r f -> f a -> IO a

data ObjectOf :: (* -> *) -> * where
  ObjectOf :: (forall a . f a -> IO a) -> ObjectOf f

instance Remote ObjectOf where
  (#) (ObjectOf g) f = g f

--data Object (f :: * -> *) = O

--(#) :: Object f -> f a -> IO a
--(#) = undefined

class Setter a f | f -> a where
   set :: a -> f ()

class Getter a f | f -> a where
   get :: f a
   
-------------------------------

class (Setter a f, Getter a f) => Variable a f | f -> a

data TheVariable (s :: *) (a :: *) where
  Set :: s -> TheVariable s ()
  Get :: TheVariable s s

instance Setter a (TheVariable a) where
  set = Set
instance Getter a (TheVariable a) where
  get = Get
instance Variable a (TheVariable a) where

newVariable :: a -> IO (ObjectOf (TheVariable a))
newVariable a = do
        ref <- newIORef a
        return $ ObjectOf $ \ f -> case f of
                                    Set x -> writeIORef ref x
                                    Get -> readIORef ref

foo = do
  o <- newVariable (9 :: Int)
  v1 <- o # get
  inc o
  v2 <- o # get
  print (v1,v2)

  
inc :: (Remote o, Variable Int f) => o f -> IO ()
inc o = do
  v <- o # get
  o # set (v + 1 :: Int)


