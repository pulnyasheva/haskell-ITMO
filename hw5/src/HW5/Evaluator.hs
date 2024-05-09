module HW5.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib as Zlib
import Codec.Serialise ()
import qualified Codec.Serialise as Sr
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (toList)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ()
import Data.Ratio
import Data.Semigroup (stimes)
import Data.Sequence (Seq, ViewL (..))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Data.Traversable ()
import Data.Word (Word8)
import HW5.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (..), HiValue (..))
import qualified Text.Read as R

eval ::  HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ evalExpr expr

evalExpr :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalExpr (HiExprValue val)                    = return val
evalExpr (HiExprApply (HiExprValue fun) args) = evalValue fun args
evalExpr (HiExprApply expr args)              = evalApply expr args
evalExpr (HiExprRun expr)                     = evalRun expr
evalExpr (HiExprDict args)                    = evalDict args

evalApply :: HiMonad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
evalApply expr args  = do
  valFun <- evalExpr expr
  evalExpr (HiExprApply (HiExprValue valFun) args)

evalRun :: HiMonad m => HiExpr ->  ExceptT HiError m HiValue
evalRun expr = do
  val <- evalExpr expr
  case val of
    (HiValueAction action) -> lift $ runAction action
    HiValueNull            -> throwError HiErrorInvalidArgument
    _                      -> return val

evalValue :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalValue (HiValueFunction fun) args        = evalFun fun args
evalValue (HiValueString text) [arg]        = evalIndex (TypeText text) arg
evalValue (HiValueString text) [arg1, arg2] = evalSlice (TypeText text) arg1 arg2
evalValue (HiValueList list) [arg]          = evalIndex (TypeList list) arg
evalValue (HiValueList list) [arg1, arg2]   = evalSlice (TypeList list) arg1 arg2
evalValue (HiValueBytes bytes) [arg]        = evalIndex (TypeBytes bytes) arg
evalValue (HiValueBytes bytes) [arg1, arg2] = evalSlice(TypeBytes bytes) arg1 arg2
evalValue (HiValueDict dict) [arg]          = evalElemFromKey dict arg
evalValue _ _                               = throwError HiErrorInvalidFunction

evalFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalFun HiFunAdd [arg1, arg2] = evalAdd arg1 arg2
evalFun HiFunMul [arg1, arg2] = evalMul arg1 arg2
evalFun HiFunSub [arg1, arg2] = evalSub arg1 arg2
evalFun HiFunDiv [arg1, arg2] = evalDiv arg1 arg2
evalFun HiFunNot [arg] = evalUnaryOp (FunBoolUn not) arg
evalFun HiFunAnd [arg1, arg2] = evalAnd arg1 arg2
evalFun HiFunOr [arg1, arg2] = evalOr arg1 arg2
evalFun HiFunLessThan[arg1, arg2] = evalLessThan arg1 arg2
evalFun HiFunGreaterThan [arg1, arg2] = evalLessThan arg2 arg1
evalFun HiFunEquals [arg1, arg2] = evalEquals arg1 arg2
evalFun HiFunNotLessThan [arg1, arg2] = notBool $ evalLessThan arg1 arg2
evalFun HiFunNotGreaterThan [arg1, arg2] = notBool $ evalLessThan arg2 arg1
evalFun HiFunNotEquals [arg1, arg2] = notBool $ evalEquals arg1 arg2
evalFun HiFunIf [arg1, arg2, arg3] = evalIf arg1 arg2 arg3
evalFun HiFunLength [arg] = evalLength arg
evalFun HiFunToUpper [arg] = evalUnaryOp (FunString T.toUpper) arg
evalFun HiFunToLower [arg] = evalUnaryOp (FunString T.toLower) arg
evalFun HiFunReverse [arg] = evalReverse arg
evalFun HiFunTrim [arg] = evalUnaryOp (FunString T.strip) arg
evalFun HiFunList args = evalList args
evalFun HiFunRange [arg1, arg2] = evalRange arg1 arg2
evalFun HiFunFold [arg1, arg2] = evalFold arg1 arg2
evalFun HiFunPackBytes [arg] = evalPack arg
evalFun HiFunUnpackBytes [arg] = evalUnpack arg
evalFun HiFunEncodeUtf8 [arg] = evalEncodeUtf8 arg
evalFun HiFunDecodeUtf8 [arg] = evalUnaryOp (FunDecode decodeUtf8) arg
evalFun HiFunZip [arg] = evalUnaryOp (FunBytes compressBytes) arg
evalFun HiFunUnzip [arg] = evalUnaryOp (FunBytes decompressBytes) arg
evalFun HiFunSerialise [arg] = evalUnaryOp (FunSerialise (LB.toStrict . Sr.serialise)) arg
evalFun HiFunDeserialise [arg] = evalUnaryOp (FunDeserialise (Sr.deserialise . LB.fromStrict)) arg
evalFun HiFunRead [arg] = evalRead arg
evalFun HiFunWrite [arg1, arg2] = evalWrite arg1 arg2
evalFun HiFunMkDir [arg] = evalMkDir arg
evalFun HiFunChDir [arg] = evalChDir arg
evalFun HiFunParseTime [arg] = evalTime arg
evalFun HiFunRand [arg1, arg2] = evalRand arg1 arg2
evalFun HiFunEcho [arg] = evalEcho arg
evalFun HiFunCount [arg] = evalCount arg
evalFun HiFunKeys [arg] = evalKeys arg
evalFun HiFunValues [arg] = evalValues arg
evalFun HiFunInvert [arg] = evalInvert arg
evalFun _ _ = throwError HiErrorArityMismatch

data FunUn = FunString (Text -> Text)
              | FunStringToRational (Text -> Rational)
              | FunBoolUn (Bool -> Bool)
              | FunBytes (ByteString -> ByteString)
              | FunSerialise (HiValue -> ByteString)
              | FunDeserialise (ByteString -> HiValue)
              | FunEncode (Text -> ByteString)
              | FunDecode (ByteString -> HiValue)

data Type = TypeText Text | TypeList (Seq HiValue) | TypeBytes ByteString

evalUnaryOp :: HiMonad m => FunUn -> HiExpr -> ExceptT HiError m HiValue
evalUnaryOp fun arg = do
  val <- evalExpr arg
  case (val, fun) of
    (HiValueString x, FunString op)           -> return $ HiValueString (op x)
    (HiValueString x, FunStringToRational op) -> return $ HiValueNumber (op x)
    (HiValueBool x, FunBoolUn op)             -> return $ HiValueBool (op x)
    (HiValueBytes x, FunBytes op)             -> return $ HiValueBytes (op x)
    (x, FunSerialise op)                      -> return $ HiValueBytes (op x)
    (HiValueBytes x, FunDeserialise op)       -> return (op x)
    (HiValueString x, FunEncode op)           -> return $ HiValueBytes (op x)
    (HiValueBytes x, FunDecode op)            -> return (op x)
    _                                         -> throwError HiErrorInvalidArgument

evalAdd :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalAdd arg1 arg2 = do
  val1 <- evalExpr arg1
  val2 <- evalExpr arg2
  case (val1, val2) of
    (HiValueNumber x, HiValueNumber y) -> return $ HiValueNumber (x + y)
    (HiValueString x, HiValueString y) -> return $ HiValueString (x <> y)
    (HiValueList x, HiValueList y)     -> return $ HiValueList (x <> y)
    (HiValueBytes x, HiValueBytes y)   -> return $ HiValueBytes (x <> y)
    (HiValueTime x, HiValueNumber y)   -> return $ HiValueTime (addUTCTime (realToFrac y) x)
    _                                  -> throwError HiErrorInvalidArgument

evalSub :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalSub arg1 arg2 = do
  val1 <- evalExpr arg1
  val2 <- evalExpr arg2
  case (val1, val2) of
    (HiValueNumber x, HiValueNumber y) -> return $ HiValueNumber (x - y)
    (HiValueTime x, HiValueTime y)     -> return $ HiValueNumber $ toRational (diffUTCTime x y)
    _                                  -> throwError HiErrorInvalidArgument

evalMul :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalMul arg1 arg2 = do
  val1 <- evalExpr arg1
  val2 <- evalExpr arg2
  case (val1, val2) of
    (HiValueNumber x, HiValueNumber y) -> return $ HiValueNumber (x * y)
    (HiValueString x, HiValueNumber y) -> repeatTextRational x y
    (HiValueNumber x, HiValueString y) -> repeatTextRational y x
    (HiValueList x, HiValueNumber y)   -> repeatListRational x y
    (HiValueNumber x, HiValueList y)   -> repeatListRational y x
    (HiValueBytes x, HiValueNumber y)  -> repeatBytesRational x y
    (HiValueNumber x, HiValueBytes y)  -> repeatBytesRational y x
    _                                  -> throwError HiErrorInvalidArgument

evalDiv :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalDiv arg1 arg2 = do
  val1 <- evalExpr arg1
  val2 <- evalExpr arg2
  case (val1, val2) of
    (HiValueNumber x, HiValueNumber y) ->
      if y == 0
      then throwError HiErrorDivideByZero
      else return (HiValueNumber (x / y))
    (HiValueString x, HiValueString y) -> return $ HiValueString (x <> T.pack "/"  <> y)
    _ -> throwError HiErrorInvalidArgument

evalOrd :: HiMonad m => HiExpr -> HiExpr -> (HiValue -> HiValue -> Bool) -> ExceptT HiError m HiValue
evalOrd arg1 arg2 op = do
  val1 <- evalExpr arg1
  val2 <- evalExpr arg2
  return $ HiValueBool (op val1 val2)

evalLessThan :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalLessThan arg1 arg2 = evalOrd arg1 arg2 (<)

evalEquals :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalEquals arg1 arg2 = evalOrd arg1 arg2 (==)

evalAnd :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalAnd arg1 arg2 = do
  val1 <- evalExpr arg1
  case val1 of
    (HiValueBool False) -> return val1
    HiValueNull         -> return val1
    _                   -> evalExpr arg2

evalOr :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalOr arg1 arg2 = do
  val1 <- evalExpr arg1
  case val1 of
    (HiValueBool False) -> evalExpr arg2
    HiValueNull         -> evalExpr arg2
    _                   -> return val1

notBool :: HiMonad m => ExceptT HiError m HiValue -> ExceptT HiError m HiValue
notBool val = do
  hiVal <- val
  case hiVal of
    HiValueBool b -> return $ HiValueBool (not b)
    _             -> throwError HiErrorInvalidArgument

evalIf :: HiMonad m => HiExpr -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalIf arg1 arg2 arg3 = do
  val1 <- evalExpr arg1
  case val1 of
    HiValueBool True  -> evalExpr arg2
    HiValueBool False -> evalExpr arg3
    _                 -> throwError HiErrorInvalidArgument

evalLength :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalLength arg = do
  val <- evalExpr arg
  case val of
    (HiValueString x) -> return $ HiValueNumber $ textLength x
    (HiValueList x)   -> return $ HiValueNumber $ listLength x
    _                 -> throwError HiErrorInvalidArgument

evalReverse :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalReverse arg = do
  val <- evalExpr arg
  case val of
    (HiValueString x) -> return $ HiValueString $ T.reverse x
    (HiValueList x)   -> return $ HiValueList $ S.reverse x
    _                 -> throwError HiErrorInvalidArgument

evalIndex :: HiMonad m => Type -> HiExpr -> ExceptT HiError m HiValue
evalIndex t arg = do
  val <- evalExpr arg
  case val of
    (HiValueNumber x) -> case t of
      (TypeText text)   -> textIndex text x
      (TypeList list)   -> listIndex list x
      (TypeBytes bytes) -> bytesIndex bytes x
    _                 -> throwError HiErrorInvalidArgument

evalSlice :: HiMonad m => Type -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalSlice t arg1 arg2 = do
  val1 <- evalExpr arg1
  val2 <- evalExpr arg2
  case (val1, val2) of
    (HiValueNumber x, HiValueNumber y) -> typeToHiValue t x y False
    (HiValueNumber x, HiValueNull)     -> typeToHiValue t x 0 True
    (HiValueNull, HiValueNumber y)     -> typeToHiValue t 0 y False
    _                                  -> throwError HiErrorInvalidArgument

evalList :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalList args = do
  values <- traverse evalExpr args
  return $ HiValueList $ S.fromList values

evalRange :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalRange arg1 arg2 = do
  val1 <- evalExpr arg1
  val2 <- evalExpr arg2
  case (val1, val2) of
    (HiValueNumber x, HiValueNumber y) -> return $ HiValueList $ S.fromList (map HiValueNumber [x, x + 1 .. y])
    _                                  -> throwError HiErrorInvalidArgument

evalFold :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalFold arg1 arg2 = do
  val1 <- evalExpr arg1
  val2 <- evalExpr arg2
  case (val1, val2) of
    (fun@(HiValueFunction _), HiValueList list) -> case S.viewl list of
      EmptyL -> throwError HiErrorInvalidArgument
      x :< xs -> fold (\a b -> evalExpr (HiExprApply (HiExprValue fun) [HiExprValue a, HiExprValue b])) x xs
    _                                             -> throwError HiErrorInvalidArgument

evalPack :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalPack arg = do
  val <- evalExpr arg
  case val of
    (HiValueList list) -> do
      res <- lift $ runExceptT $ packByte list []
      case res of
        Left err  -> throwError err
        Right ans -> return $ HiValueBytes $ B.pack ans
    _                  -> throwError HiErrorInvalidArgument

evalUnpack :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalUnpack arg = do
  val <- evalExpr arg
  case val of
    (HiValueBytes bytes) -> return $ HiValueList (S.fromList $ map word8ToHiValue (B.unpack bytes))
    _                    -> throwError HiErrorInvalidArgument

evalEncodeUtf8 :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalEncodeUtf8 = evalUnaryOp (FunEncode encodeUtf8)

evalActionOnePath :: HiMonad m => (FilePath -> HiAction) -> HiExpr -> ExceptT HiError m HiValue
evalActionOnePath actionConstructor arg = do
  val <- evalExpr arg
  case val of
    (HiValueString text) -> return $ HiValueAction $ actionConstructor $ T.unpack text
    _                    -> throwError HiErrorInvalidArgument

evalRead :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalRead = evalActionOnePath HiActionRead

evalWrite :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalWrite arg1 arg2 = do
  val1 <- evalExpr arg1
  val2 <-  evalExpr arg2
  val2' <- evalEncodeUtf8 (HiExprValue val2)
  case (val1, val2') of
    (HiValueString text, HiValueBytes bytes) -> return $ HiValueAction $ HiActionWrite (T.unpack text) bytes
    _                                        -> throwError HiErrorInvalidArgument

evalMkDir :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalMkDir = evalActionOnePath HiActionMkDir

evalChDir :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalChDir = evalActionOnePath HiActionChDir

evalTime :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalTime arg = do
  val <- evalExpr arg
  case val of
    (HiValueString str) -> case parseUTCTime (T.unpack str) of
      Just time -> return $ HiValueTime time
      Nothing   -> return HiValueNull
    _                   -> throwError HiErrorInvalidArgument

evalRand :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalRand arg1 arg2 = do
  val1 <- evalExpr arg1
  val2 <- evalExpr arg2
  case (val1, val2) of
    (HiValueNumber x, HiValueNumber y) -> if isInteger x && isInteger y
      then return $ HiValueAction $ HiActionRand (fromIntegral $ numerator x) (fromIntegral $ numerator y)
      else throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

evalEcho :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalEcho arg = do
  val <- evalExpr arg
  case val of
    (HiValueString text) -> return $ HiValueAction $ HiActionEcho text
    _                    -> throwError HiErrorInvalidArgument

evalDict :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m HiValue
evalDict args = do
  list <- pairsToListOfTuples args
  return $ HiValueDict $ Map.fromList list
  where
    pairsToListOfTuples [] = return []
    pairsToListOfTuples ((x,y):xs) = do
      val1 <- evalExpr x
      val2 <- evalExpr y
      end <- pairsToListOfTuples xs
      return ((val1, val2) : end)

evalKeys :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalKeys arg = do
  val <- evalExpr arg
  case val of
    (HiValueDict dict) -> return $ HiValueList $ S.fromList $ sort $ Map.keys dict
    _                  -> throwError HiErrorInvalidArgument

evalValues :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalValues arg = do
  val <- evalExpr arg
  case val of
    (HiValueDict dict) -> return $ HiValueList $ S.fromList $ map (dict Map.!) (sort $ Map.keys dict)
    _ -> throwError HiErrorInvalidArgument

evalElemFromKey :: HiMonad m => Map.Map HiValue HiValue -> HiExpr -> ExceptT HiError m HiValue
evalElemFromKey dict arg = do
  val <- evalExpr arg
  case Map.lookup val dict of
    Just res -> return res
    Nothing  -> return HiValueNull

evalCount :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalCount arg = do
  val <- evalExpr arg
  case val of
    (HiValueList list)   -> return $ listToDict $ toList list
    (HiValueBytes bytes) -> return $ listToDict $ map (HiValueNumber . toRational) (B.unpack bytes)
    (HiValueString text) -> return $ listToDict $ map (HiValueString . T.singleton) (T.unpack text)
    _                    -> throwError HiErrorInvalidArgument


evalInvert :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalInvert arg = do
  val <- evalExpr arg
  case val of
    (HiValueDict dict) -> return $ HiValueDict $ invertMap dict
    _                  -> throwError HiErrorInvalidArgument

invertMap :: Map HiValue HiValue -> Map HiValue HiValue
invertMap dict = Map.map HiValueList (Map.fromListWith (<>) [(v, S.singleton k) | (k, v) <- Map.toList dict])

typeToHiValue :: HiMonad m => Type -> Rational -> Rational -> Bool -> ExceptT HiError m HiValue
typeToHiValue t start y len = case t of
  (TypeText text)   -> textSlice text start (getEnd y len text textLength)
  (TypeList list)   -> listSlice list start (getEnd y len list listLength)
  (TypeBytes bytes) -> bytesSlice bytes start (getEnd y len bytes bytesLength)

getEnd :: Rational -> Bool -> a -> (a -> Rational) -> Rational
getEnd y b a f = if b
               then f a
               else y

listToDict :: [HiValue] -> HiValue
listToDict list = HiValueDict $ Map.map HiValueNumber $ foldr (\x -> Map.insertWith (+) x 1) Map.empty list

parseUTCTime :: String -> Maybe UTCTime
parseUTCTime = R.readMaybe

decodeUtf8 :: ByteString -> HiValue
decodeUtf8 bytes = case decodeUtf8' bytes of
  Left _     -> HiValueNull
  Right text -> HiValueString text

fold :: HiMonad m => (HiValue -> HiValue -> ExceptT HiError m HiValue) -> HiValue -> Seq HiValue -> ExceptT HiError m HiValue
fold f acc list = case S.viewl list of
  EmptyL  -> return acc
  x :< xs -> do
    res <- lift $ runExceptT (f acc x)
    case res of
      Left err  -> throwError err
      Right val -> fold f val xs

repeatWithRational :: HiMonad m => (Integer -> a -> a) -> (a -> HiValue) -> a -> Rational -> ExceptT HiError m HiValue
repeatWithRational repeatFun valueFun x n = if isInteger n
  then return $ valueFun $ repeatFun (numerator n) x
  else throwError HiErrorInvalidArgument

repeatTextRational :: HiMonad m => Text -> Rational -> ExceptT HiError m HiValue
repeatTextRational = repeatWithRational stimes HiValueString

repeatListRational :: HiMonad m => Seq HiValue -> Rational -> ExceptT HiError m HiValue
repeatListRational l = repeatWithRational stimes (HiValueList . S.fromList) (toList l)

repeatBytesRational :: HiMonad m => ByteString -> Rational -> ExceptT HiError m HiValue
repeatBytesRational = repeatWithRational stimes HiValueBytes

textLength :: Text -> Rational
textLength = fromIntegral . T.length

listLength :: Seq HiValue -> Rational
listLength = fromIntegral . S.length

bytesLength :: ByteString -> Rational
bytesLength = fromIntegral . B.length

indexWithRational :: HiMonad m => (a -> Int -> b) -> (b -> HiValue) -> a -> Rational -> Int -> ExceptT HiError m HiValue
indexWithRational indexFun valueFun x index len = case quotRem (numerator index) (denominator index) of
  (q, 0) -> if (q >= 0) && (q < fromIntegral len)
            then return $ valueFun $ indexFun x (fromIntegral q)
            else return HiValueNull
  _      -> throwError HiErrorInvalidArgument

textIndex :: HiMonad m => Text -> Rational -> ExceptT HiError m HiValue
textIndex t n = indexWithRational T.index (HiValueString . T.singleton) t n (T.length t)

listIndex :: HiMonad m => Seq HiValue -> Rational -> ExceptT HiError m HiValue
listIndex l n = indexWithRational S.index id l n (S.length l)

bytesIndex :: HiMonad m => ByteString -> Rational -> ExceptT HiError m HiValue
bytesIndex b n = indexWithRational B.index word8ToHiValue b n (B.length b)

sliceWithRational :: HiMonad m => Integer -> (Int -> a -> a) -> (Int -> a -> a) -> (a -> HiValue) -> a -> Rational -> Rational -> a -> ExceptT HiError m HiValue
sliceWithRational len takeFun dropFun valueFn x start end emp =
  case (quotRem (numerator start) (denominator start), quotRem (numerator end) (denominator end))  of
    ((s, 0), (e, 0)) -> let start1 = if s < 0
                                     then s + len
                                     else s
                            end1 = if e < 0
                                   then e + len
                                   else e
                        in if (start1 >= 0) && (start1 <= len)
                          then let lenSlice
                                       | (end1 - start1) < 0 = 0
                                       | end1 > len = (len - start1)
                                       | otherwise = end1 - start1
                               in return $ valueFn $ takeFun (fromIntegral lenSlice) $ dropFun (fromIntegral start1) x
                          else return $ valueFn emp
    _ -> throwError HiErrorInvalidArgument

textSlice :: HiMonad m => Text -> Rational -> Rational -> ExceptT HiError m HiValue
textSlice t s e = sliceWithRational (fromIntegral $ T.length t) T.take T.drop HiValueString t s e T.empty

listSlice :: HiMonad m => Seq HiValue -> Rational -> Rational -> ExceptT HiError m HiValue
listSlice l s e = sliceWithRational (fromIntegral $ S.length l) S.take S.drop HiValueList l s e S.empty

bytesSlice :: HiMonad m => ByteString -> Rational -> Rational -> ExceptT HiError m HiValue
bytesSlice b s e = sliceWithRational (fromIntegral $ B.length b) B.take B.drop HiValueBytes b s e B.empty

packByte :: HiMonad m => Seq HiValue -> [Word8] -> ExceptT HiError m [Word8]
packByte list acc = case S.viewl list of
  EmptyL  -> return acc
  x :< xs -> do
    res <- lift $ runExceptT (case x of
      (HiValueNumber y) -> case quotRem (numerator y) (denominator y) of
        (q, 0) -> if q >= 0 && q <= 255
                  then return (acc ++ [fromIntegral q :: Word8])
                  else throwError HiErrorInvalidArgument
        _ -> throwError HiErrorInvalidArgument
      _                 -> throwError HiErrorInvalidArgument)
    case res of
      Left err  -> throwError err
      Right val -> packByte xs val


word8ToHiValue :: Word8 -> HiValue
word8ToHiValue word = HiValueNumber (toRational $ toInteger word)

compressBytes :: ByteString -> ByteString
compressBytes = LB.toStrict . Zlib.compressWith Zlib.defaultCompressParams { Zlib.compressLevel = Zlib.bestCompression } . LB.fromStrict

decompressBytes :: ByteString -> ByteString
decompressBytes = LB.toStrict . Zlib.decompress . LB.fromStrict

isInteger :: Rational -> Bool
isInteger x = denominator x == 1


