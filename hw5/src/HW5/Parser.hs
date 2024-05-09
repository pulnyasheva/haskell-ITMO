module HW5.Parser
  ( parse
  ) where

import Control.Monad.Combinators.Expr
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import Data.Ratio ()
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (Parsec, between, choice, eof, many, manyTill, notFollowedBy, oneOf,
                        runParser, satisfy, sepBy, sepBy1, skipMany, some, try, (<|>))
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)

import HW5.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (parseWithSpaces parseHiExpr <* eof) ""

type Parser = Parsec Void String

skipSpaces :: Parser ()
skipSpaces = skipMany space1

parseWithSpaces :: Parser a -> Parser a
parseWithSpaces p = skipSpaces *> p <* skipSpaces

parseStringWithSpaces :: String -> Parser String
parseStringWithSpaces str = parseWithSpaces $ string str

staples :: String -> String -> Parser a -> Parser a
staples s1 s2 = between (string s1) (string s2)

parens :: Parser a -> Parser a
parens = staples "(" ")"

brackets :: Parser a -> Parser a
brackets = staples "[" "]"

braces :: Parser a -> Parser a
braces = staples "{" "}"

parseHiFun :: Parser HiFun
parseHiFun = choice [
  HiFunDiv <$ string "div",
  HiFunMul <$ string "mul",
  HiFunAdd <$ string "add",
  HiFunSub <$ string "sub",
  HiFunAnd <$ string "and",
  HiFunOr <$ string "or",
  HiFunLessThan <$ string "less-than",
  HiFunGreaterThan <$ string "greater-than",
  HiFunEquals <$ string "equals",
  HiFunNotLessThan <$ string "not-less-than",
  HiFunNotGreaterThan <$ string "not-greater-than",
  HiFunNotEquals <$ string "not-equals",
  HiFunNot <$ string "not",
  HiFunIf <$ string "if",
  HiFunLength <$ string "length",
  HiFunToUpper <$ string "to-upper",
  HiFunToLower <$ string "to-lower",
  HiFunReverse <$ string "reverse",
  HiFunTrim <$ string "trim",
  HiFunList <$ string "list",
  HiFunRange <$ string "range",
  HiFunFold <$ string "fold",
  HiFunPackBytes <$ string "pack-bytes",
  HiFunUnpackBytes <$ string "unpack-bytes",
  HiFunEncodeUtf8 <$ string "encode-utf8",
  HiFunDecodeUtf8 <$ string "decode-utf8",
  HiFunZip <$ string "zip",
  HiFunUnzip <$ string "unzip",
  HiFunSerialise <$ string "serialise",
  HiFunDeserialise <$ string "deserialise",
  HiFunRead <$ string "read",
  HiFunWrite <$ string "write",
  HiFunMkDir <$ string "mkdir",
  HiFunChDir <$ string "cd",
  HiFunParseTime <$ string "parse-time",
  HiFunRand <$ string "rand",
  HiFunEcho <$ string "echo",
  HiFunCount <$ string "count",
  HiFunKeys <$ string "keys",
  HiFunValues <$ string "values",
  HiFunInvert <$ string "invert"
  ]

parseHiValue :: Parser HiValue
parseHiValue = choice [
  HiValueFunction <$> parseHiFun,
  HiValueNumber <$> parseNumber,
  HiValueBool <$> parseBool,
  parseNull,
  HiValueString <$> parseString,
  HiValueBytes <$> parseBytes,
  HiValueAction <$> parseAction
  ]

parseNumber :: Parser Rational
parseNumber = do
  sign <- try ( Just <$> string "-") <|> return Nothing
  num <- L.scientific
  return $ case sign of
    Just _  -> negate (toRational num)
    Nothing -> toRational num

parseBool :: Parser Bool
parseBool = choice [
  True <$ string "true",
  False <$ string "false"
  ]

parseNull :: Parser HiValue
parseNull = HiValueNull <$ string "null"

parseString :: Parser Text
parseString = do
  _ <- char '"'
  Text.pack <$> manyTill L.charLiteral (char '"')

parseBytes :: Parser ByteString
parseBytes = do
  _ <- string "[#"
  bytes <- many $ try parseHex
  space1
  _ <- string "#]"
  return (ByteString.pack bytes)

parseHex :: Parser Word8
parseHex = do
  space1
  d1 <- oneOf "0123456789abcdef"
  d2 <- oneOf "0123456789abcdef"
  return (read ("0x" ++ [d1, d2]))

parseAction :: Parser HiAction
parseAction = choice [
  HiActionCwd <$ string "cwd",
  HiActionNow <$ string "now"
  ]

parseHiExprValue :: Parser HiExpr
parseHiExprValue = HiExprValue <$> parseHiValue

parseList :: Parser HiExpr
parseList = do
  args <- brackets $ sepBy parseHiExpr (parseStringWithSpaces ",")
  return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) args

parseMap :: Parser HiExpr
parseMap = do
  args <- braces $ sepBy parsePair (parseStringWithSpaces ",")
  return $ HiExprDict args

parsePair :: Parser (HiExpr, HiExpr)
parsePair = do
  key <- parseHiExpr
  _ <- string ":"
  value <- parseHiExpr
  return (key, value)

parseHiTerm :: Parser HiExpr
parseHiTerm = choice [
  parens parseHiExpr,
  parseHiExprValue,
  parseList,
  parseMap
  ]

parseHiExpr :: Parser HiExpr
parseHiExpr = makeExprParser (parseWithSpaces parseHiTerm) operators

operators :: [[Operator Parser HiExpr]]
operators =
  [ [Postfix parseManyUnaryOp]
  , [InfixL (try parseDiv >> return (\x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunDiv) [x, y]))
  ,  InfixL (string "*" >> return (\x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunMul) [x, y]))]
  , [InfixL (string "+" >> return (\x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunAdd) [x, y]))
  ,  InfixL (string "-" >> return (\x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunSub) [x, y]))]
  , [InfixN (string ">=" >> return (\x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunNotLessThan) [x, y]))
  ,  InfixN (string "<=" >> return (\x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunNotGreaterThan) [x, y]))
  ,  InfixN (string "==" >> return (\x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunEquals) [x, y]))
  ,  InfixN (string "<" >> return (\x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunLessThan) [x, y]))
  ,  InfixN (string ">" >> return (\x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunGreaterThan) [x, y]))
  ,  InfixN (string "/=" >> return (\x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunNotEquals) [x, y]))]
  , [InfixR (string "&&" >> return (\x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunAnd) [x, y]))]
  , [InfixR (string "||" >> return (\x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunOr) [x, y]))]
  ]

parseDiv :: Parser String
parseDiv = do
  result <- string "/"
  notFollowedBy (string "=")
  return result

parseRun :: Parser (HiExpr -> HiExpr)
parseRun = do
  _ <- string "!"
  return HiExprRun

parsePoint :: Parser (HiExpr -> HiExpr)
parsePoint = do
  _ <- string "."
  str <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
  return (\x -> HiExprApply x [HiExprValue $ HiValueString $ Text.pack (intercalate "-" str)])

parseParens :: Parser (HiExpr -> HiExpr)
parseParens = do
  _ <- string "("
  expr <- parseHiExpr
  _ <- string ")"
  return (\x -> HiExprApply x [expr])

parseParensComma :: Parser (HiExpr -> HiExpr)
parseParensComma = do
  args <- parens $ sepBy parseHiExpr (parseStringWithSpaces ",")
  return (`HiExprApply` args)


parseUnaryOp :: Parser (HiExpr -> HiExpr)
parseUnaryOp = choice
    [parseWithSpaces parseRun,
    parseWithSpaces parsePoint,
    parseWithSpaces (try parseParens <|> parseParensComma)
    ]

parseManyUnaryOp :: Parser (HiExpr -> HiExpr)
parseManyUnaryOp = foldr1 (flip (.))  <$> some parseUnaryOp
