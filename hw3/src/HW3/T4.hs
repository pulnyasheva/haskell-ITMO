module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f state = S (\s -> let (a :# s1) = runS state s
                                b         = f a
                            in b :# s1)

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState state = S (\s -> let (stateInternal :# s1) = runS state s
                           in runS stateInternal s1)

modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) state1 state2 = S (\s -> let (f :# s1) = runS state1 s
                                     (a :# s2) = runS state2 s1
                                 in f a :# s2)

instance Monad (State s) where
  (>>=) state f = S (\s -> let (a :# s1) = runS state s
                               state2    = runS (f a) s1
                           in state2)

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

eval :: Expr -> State [Prim Double] Double
eval (Val x) = pure x
eval (Op op) = do evalOp op

evalOp :: Prim Expr -> State [Prim Double] Double
evalOp (Add x y) = applyOpBin (+) "Add" x y
evalOp (Sub x y) = applyOpBin (-) "Sub" x y
evalOp (Mul x y) = applyOpBin (*) "Mul" x y
evalOp (Div x y) = applyOpBin (/) "Div" x y
evalOp (Abs x)   = applyOpUn abs "Abs" x
evalOp (Sgn x)   = applyOpUn signum "Sgn" x

createOpBin :: String -> a -> a -> Prim a
createOpBin op x y = case op of
                      "Add" -> Add x y
                      "Sub" -> Sub x y
                      "Mul" -> Mul x y
                      "Div" -> Div x y
                      _     -> error "Unknown operationBin"

createOpUn :: String -> a -> Prim a
createOpUn op x = case op of
                    "Abs" -> Abs x
                    "Sgn" -> Sgn x
                    _     -> error "Unknown operationUn"

applyOpBin :: (Double -> Double -> Double) -> String -> Expr -> Expr -> State [Prim Double] Double
applyOpBin op name x y = do
  resX <- eval x
  resY <- eval y
  let res = op resX resY
  modifyState (createOpBin name resX resY :)
  return res

applyOpUn :: (Double -> Double) -> String -> Expr -> State [Prim Double] Double
applyOpUn op name x = do
  resX <- eval x
  let res = op resX
  modifyState (createOpUn name resX :)
  return res






