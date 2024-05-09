{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  , pEof
  ) where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isSpace)
import Numeric.Natural (Natural)

import HW4.T1 (ExceptState (..))
import HW4.Types

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P (ES p)) s = case p (0, s) of
  Error e               -> Error e
  Success (result :# _) -> Success result

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P p1) (P p2) = P $ ES $ \(pos, s) ->
    case runES p1 (pos, s) of
      Error _     -> runES p2 (pos, s)
      Success res -> Success res

-- No metohds
instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, s))
    _  -> Error (ErrorAtPos pos)

parseExpr :: String -> Except ParseError Expr
parseExpr = runP (pExpr <* pEof)

queue :: Parser a -> Parser (a -> a -> a) -> Parser a
queue p op = do
  _ <- pSpaces
  x <- p
  _ <- pSpaces
  rest x
  where
    rest x = (do
      _ <- pSpaces
      f <- op
      _ <- pSpaces
      y <- p
      _ <- pSpaces
      rest (f x y))
      <|> return x

pExpr :: Parser Expr
pExpr = queue pMulDiv opAddSub

pMulDiv :: Parser Expr
pMulDiv = queue pFactor opMulDiv

pFactor :: Parser Expr
pFactor = pStaples pExpr <|> pNumber

pStaples :: Parser a -> Parser a
pStaples p = char '(' *> p <* char ')'

pNumber :: Parser Expr
pNumber = Val <$> (readDouble <|> readInt)

readDouble :: Parser Double
readDouble = do
 intPart <- some $ mfilter isDigit pChar
 _ <- char '.'
 decPart <- many $ mfilter isDigit pChar
 let num = read (intPart ++ "." ++ decPart) :: Double
 return num

readInt :: Parser Double
readInt = do
  intPart <- some $ mfilter isDigit pChar
  let num = read intPart :: Double
  return num

opAddSub :: Parser (Expr -> Expr -> Expr)
opAddSub = (char '+' *> return (\x y -> Op (Add x y))) <|> (char '-' *> return (\x y -> Op (Sub x y)))

opMulDiv :: Parser (Expr -> Expr -> Expr)
opMulDiv = (char '*' *> return (\x y -> Op (Mul x y))) <|> (char '/' *> return (\x y -> Op (Div x y)))

char :: Char -> Parser Char
char c = mfilter (== c) pChar

pSpaces :: Parser String
pSpaces = many (mfilter isSpace pChar)


