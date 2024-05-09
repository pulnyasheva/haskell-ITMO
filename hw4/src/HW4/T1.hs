module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import Control.Monad
import HW4.Types

newtype ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success (f a)

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f es = ES $ \s -> mapExcept (mapAnnotated f) $ runES es s

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState es  = ES $ \s ->
  case runES es s of
    Error e             -> Error e
    Success (es1 :# s1) -> runES es1 s1

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = ap

instance Monad (ExceptState e s) where
  (>>=) m f = joinExceptState $ fmap f m

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = pure x
eval (Op (Add x y)) = do
  vx <- eval x
  vy <- eval y
  modifyExceptState (Add vx vy :)
  return $ vx + vy
eval (Op (Sub x y)) = do
  vx <- eval x
  vy <- eval y
  modifyExceptState (Sub vx vy :)
  return $ vx - vy
eval (Op (Mul x y)) = do
  vx <- eval x
  vy <- eval y
  modifyExceptState (Mul vx vy :)
  return $ vx * vy
eval (Op (Div x y)) = do
  vx <- eval x
  vy <- eval y
  if vy == 0
  then throwExceptState DivideByZero
  else do
    modifyExceptState (Div vx vy :)
    return $ vx / vy
eval (Op (Abs x)) = do
  vx <- eval x
  modifyExceptState (Abs vx :)
  return $ abs vx
eval (Op (Sgn x)) = do
  vx <- eval x
  modifyExceptState (Sgn vx :)
  return $ signum vx

