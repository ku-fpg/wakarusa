{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Natural
import Control.Transformation
import Control.Monad.Normal
import Control.Applicative
import Data.IORef
import Control.Monad.State

newtype Small a = Small (MONAD Prim a)
  deriving (Functor, Applicative, Monad)

-- The only thing you can do to an Array is call a method, which are ArrayPrim's.
newtype Array a = Array (ArrayPrim a ~> Small)

instance Transformation (ArrayPrim a) Small (Array a) where
  (Array t) # prim = t prim

data Prim :: * -> * where
   NewArray  :: Int           -> Prim (Array a)
   ArrayPrim :: ArrayId a     -> ArrayPrim a b -> Prim b

data ArrayId a = ArrayId Int

instance Show (ArrayId a) where
   show (ArrayId n) = "arr_" ++ show n

data ArrayPrim :: * -> * -> * where
   Print  ::                       ArrayPrim a ()
   Map    :: (Expr a -> Expr a) -> ArrayPrim a ()

data Expr :: * -> * where
  Lit :: Int                  -> Expr Int
  Var :: Int                  -> Expr a
  Add :: Expr Int -> Expr Int -> Expr Int

instance Show (Expr a) where
  show (Lit i) = show i
  show (Var i) = "var_" ++ show i
  show (Add e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  

instance Num (Expr Int) where
  (+) = Add
  fromInteger = Lit . fromInteger
  (*) = error "(*)"
  abs = error "abs"
  signum = error "signum"
  negate = error "negate"

--------------------------------------------------
-- Our DSL
   
new :: Int -> Small (Array a)
new = Small . liftNM . NewArray

-- Only print' because of clash with Prelude.print
print' :: ArrayPrim a ()
print' = Print

map' :: (Expr a -> Expr a) -> ArrayPrim a ()
map' = Map

-----------------------------------------
compile :: Small ~> CG
compile (Small m) = foldNM ret bind m
  where
    ret :: a -> CG a
    ret a = return a

    bind :: Prim x -> (x -> CG r) -> CG r
    bind (NewArray n) k = do
        i <- gensymCG 
        let arr = ArrayId i
        issueCG $ "Array " ++ show arr ++ " = new Array(" ++ show n ++ ")"
        k (Array $ \ f -> Small (liftNM (ArrayPrim (ArrayId i) f)))
    bind (ArrayPrim arr Print) k = do
        issueCG $ show arr ++ ".print()" 
        k ()
    bind (ArrayPrim arr (Map f)) k = do
        i <- gensymCG 
        let v = Var i
        issueCG $ show arr ++ ".map(\\ " ++ show v ++ " -> " ++ show (f v) ++ ")"
        k ()

newtype CG a = CG (State (Int,[String]) a)
  deriving (Monad, Applicative, Functor, MonadState (Int,[String]))

gensymCG :: CG Int
gensymCG = do
        (i,s) <- get 
        put (i+1,s)
        return i

issueCG :: String -> CG ()
issueCG msg = do
        (i,s) <- get 
        put (i,s ++ [msg])
        return ()

runCG :: CG () -> [String]
runCG (CG m) = snd $ execState m (0,[])

---------------------------

ex1 :: Small ()
ex1 = do
   arr :: Array Int <- new 4
   arr # print'
   arr # map' (+ 1)
   arr # print'

main = putStrLn $ unlines $ runCG $ compile $ ex1
