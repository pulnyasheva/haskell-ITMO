module HW5.Base
  ( HiError(..)
  , HiExpr(..)
  , HiFun(..)
  , HiValue(..)
  , HiMonad(..)
  , HiAction(..)
  ) where

import Codec.Serialise (Serialise, decode, encode)
import Codec.Serialise.Decoding (decodeListLen, decodeWord8)
import Codec.Serialise.Encoding (encodeListLen, encodeWord8)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime)

data HiFun = HiFunDiv
               | HiFunMul
               | HiFunAdd
               | HiFunSub
               | HiFunNot
               | HiFunAnd
               | HiFunOr
               | HiFunLessThan
               | HiFunGreaterThan
               | HiFunEquals
               | HiFunNotLessThan
               | HiFunNotGreaterThan
               | HiFunNotEquals
               | HiFunIf
               | HiFunLength
               | HiFunToUpper
               | HiFunToLower
               | HiFunReverse
               | HiFunTrim
               | HiFunList
               | HiFunRange
               | HiFunFold
               | HiFunPackBytes
               | HiFunUnpackBytes
               | HiFunEncodeUtf8
               | HiFunDecodeUtf8
               | HiFunZip
               | HiFunUnzip
               | HiFunSerialise
               | HiFunDeserialise
               | HiFunRead
               | HiFunWrite
               | HiFunMkDir
               | HiFunChDir
               | HiFunParseTime
               | HiFunRand
               | HiFunEcho
               | HiFunCount
               | HiFunKeys
               | HiFunValues
               | HiFunInvert
               deriving (Eq, Ord, Show)

data HiAction = HiActionRead FilePath
                  | HiActionWrite FilePath ByteString
                  | HiActionMkDir FilePath
                  | HiActionChDir FilePath
                  | HiActionCwd
                  | HiActionNow
                  | HiActionRand Int Int
                  | HiActionEcho Text
                  deriving (Eq, Ord, Show)

data HiValue = HiValueBool Bool
                 | HiValueNumber Rational
                 | HiValueFunction HiFun
                 | HiValueNull
                 | HiValueString Text
                 | HiValueList (Seq HiValue)
                 | HiValueBytes ByteString
                 | HiValueAction HiAction
                 | HiValueTime UTCTime
                 | HiValueDict (Map HiValue HiValue)
                 deriving (Eq, Show)

data HiExpr = HiExprValue HiValue
                | HiExprApply HiExpr [HiExpr]
                | HiExprRun HiExpr
                | HiExprDict [(HiExpr, HiExpr)]
                deriving (Show)

data HiError = HiErrorInvalidArgument
                | HiErrorInvalidFunction
                | HiErrorArityMismatch
                | HiErrorDivideByZero
                deriving (Show)

instance Ord HiValue where
  compare x y = case (x, y) of
    (HiValueBool a, HiValueBool b)         -> compare a b
    (HiValueNumber a, HiValueNumber b)     -> compare a b
    (HiValueFunction a, HiValueFunction b) -> compare a b
    (HiValueString a, HiValueString b)     -> compare a b
    (HiValueList a, HiValueList b)         -> compare a b
    (HiValueBytes a, HiValueBytes b)       -> compare a b
    (HiValueAction a, HiValueAction b)     -> compare a b
    (HiValueTime a, HiValueTime b)         -> compare a b
    (HiValueDict a, HiValueDict b)         -> compare a b
    (_ , _)                                -> compare (tag x) (tag y)
    where tag :: HiValue -> Int
          tag (HiValueBool _)     = 0
          tag (HiValueNumber _)   = 1
          tag HiValueNull         = 2
          tag (HiValueFunction _) = 3
          tag (HiValueString _)   = 4
          tag (HiValueList _)     = 5
          tag (HiValueBytes _)    = 6
          tag (HiValueAction _)   = 7
          tag (HiValueTime _)     = 8
          tag (HiValueDict _)     = 9

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

instance Serialise HiValue where
    encode (HiValueNumber x)   = encodeListLen 2 <> encodeWord8 0 <> encode x
    encode (HiValueFunction x) = encodeListLen 2 <> encodeWord8 1 <> encode x
    encode (HiValueBool x)     = encodeListLen 2 <> encodeWord8 2 <> encode x
    encode HiValueNull         = encodeListLen 1 <> encodeWord8 3
    encode (HiValueString x)   = encodeListLen 2 <> encodeWord8 4 <> encode (encodeUtf8 x)
    encode (HiValueList xs)    = encodeListLen 2 <> encodeWord8 5 <> encode (toList xs)
    encode (HiValueBytes bs)   = encodeListLen 2 <> encodeWord8 6 <> encode bs
    encode (HiValueAction ac)  = encodeListLen 2 <> encodeWord8 7 <> encode ac
    encode (HiValueTime tm)    = encodeListLen 2 <> encodeWord8 8 <> encode tm
    encode (HiValueDict dc)    = encodeListLen 2 <> encodeWord8 9 <> encode dc

    decode = do
        len <- decodeListLen
        tag <- decodeWord8
        case (len, tag) of
            (2, 0) -> HiValueNumber <$> decode
            (2, 2) -> HiValueBool <$> decode
            (2, 1) -> HiValueFunction <$> decode
            (1, 3) -> return HiValueNull
            (2, 4) -> HiValueString . decodeUtf8 <$> decode
            (2, 5) -> HiValueList . S.fromList <$> decode
            (2, 6) -> HiValueBytes <$> decode
            (2, 7) -> HiValueAction <$> decode
            (2, 8) -> HiValueTime <$> decode
            (2, 9) -> HiValueDict <$> decode
            _      -> fail "Invalid HiValue encoding"

instance Serialise HiFun where
    encode HiFunDiv            = encodeWord8 0
    encode HiFunMul            = encodeWord8 1
    encode HiFunAdd            = encodeWord8 2
    encode HiFunSub            = encodeWord8 3
    encode HiFunNot            = encodeWord8 4
    encode HiFunAnd            = encodeWord8 5
    encode HiFunOr             = encodeWord8 6
    encode HiFunLessThan       = encodeWord8 7
    encode HiFunGreaterThan    = encodeWord8 8
    encode HiFunEquals         = encodeWord8 9
    encode HiFunNotLessThan    = encodeWord8 10
    encode HiFunNotGreaterThan = encodeWord8 11
    encode HiFunNotEquals      = encodeWord8 12
    encode HiFunIf             = encodeWord8 13
    encode HiFunLength         = encodeWord8 14
    encode HiFunToUpper        = encodeWord8 15
    encode HiFunToLower        = encodeWord8 16
    encode HiFunReverse        = encodeWord8 17
    encode HiFunTrim           = encodeWord8 18
    encode HiFunList           = encodeWord8 19
    encode HiFunRange          = encodeWord8 20
    encode HiFunFold           = encodeWord8 21
    encode HiFunPackBytes      = encodeWord8 22
    encode HiFunUnpackBytes    = encodeWord8 23
    encode HiFunEncodeUtf8     = encodeWord8 24
    encode HiFunDecodeUtf8     = encodeWord8 25
    encode HiFunZip            = encodeWord8 26
    encode HiFunUnzip          = encodeWord8 27
    encode HiFunSerialise      = encodeWord8 28
    encode HiFunDeserialise    = encodeWord8 29
    encode HiFunRead           = encodeWord8 30
    encode HiFunWrite          = encodeWord8 31
    encode HiFunMkDir          = encodeWord8 32
    encode HiFunChDir          = encodeWord8 33
    encode HiFunParseTime      = encodeWord8 34
    encode HiFunRand           = encodeWord8 35
    encode HiFunEcho           = encodeWord8 36
    encode HiFunCount          = encodeWord8 37
    encode HiFunKeys           = encodeWord8 38
    encode HiFunValues         = encodeWord8 39
    encode HiFunInvert         = encodeWord8 40

    decode = do
      tag <- decodeWord8
      case tag of
        0  -> pure HiFunDiv
        1  -> pure HiFunMul
        2  -> pure HiFunAdd
        3  -> pure HiFunSub
        4  -> pure HiFunNot
        5  -> pure HiFunAnd
        6  -> pure HiFunOr
        7  -> pure HiFunLessThan
        8  -> pure HiFunGreaterThan
        9  -> pure HiFunEquals
        10 -> pure HiFunNotLessThan
        11 -> pure HiFunNotGreaterThan
        12 -> pure HiFunNotEquals
        13 -> pure HiFunIf
        14 -> pure HiFunLength
        15 -> pure HiFunToUpper
        16 -> pure HiFunToLower
        17 -> pure HiFunReverse
        18 -> pure HiFunTrim
        19 -> pure HiFunList
        20 -> pure HiFunRange
        21 -> pure HiFunFold
        22 -> pure HiFunPackBytes
        23 -> pure HiFunUnpackBytes
        24 -> pure HiFunEncodeUtf8
        25 -> pure HiFunDecodeUtf8
        26 -> pure HiFunZip
        27 -> pure HiFunUnzip
        28 -> pure HiFunSerialise
        29 -> pure HiFunDeserialise
        30 -> pure HiFunRead
        31 -> pure HiFunWrite
        32 -> pure HiFunMkDir
        33 -> pure HiFunChDir
        34 -> pure HiFunParseTime
        35 -> pure HiFunRand
        36 -> pure HiFunEcho
        37 -> pure HiFunCount
        38 -> pure HiFunKeys
        39 -> pure HiFunValues
        40 -> pure HiFunInvert
        _  -> fail "Invalid HiFun encoding"

instance Serialise HiAction where
  encode (HiActionRead fp)     = encodeListLen 2 <> encodeWord8 0 <> encode fp
  encode (HiActionWrite fp bs) = encodeListLen 3 <> encodeWord8 1 <> encode fp <> encode bs
  encode (HiActionMkDir fp)    = encodeListLen 2 <> encodeWord8 2 <> encode fp
  encode (HiActionChDir fp)    = encodeListLen 2 <> encodeWord8 3 <> encode fp
  encode HiActionCwd           = encodeListLen 1 <> encodeWord8 4
  encode HiActionNow           = encodeListLen 1 <> encodeWord8 5
  encode (HiActionRand x y)    = encodeListLen 3 <> encodeWord8 6 <> encode x <> encode y
  encode (HiActionEcho tx)     = encodeListLen 2 <> encodeWord8 7 <> encode tx

  decode = do
    len <- decodeListLen
    tag <- decodeWord8
    case (len, tag) of
      (2, 0) -> HiActionRead <$> decode
      (3, 1) -> HiActionWrite <$> decode <*> decode
      (2, 2) -> HiActionMkDir <$> decode
      (2, 3) -> HiActionChDir <$> decode
      (1, 4) -> return HiActionCwd
      (1, 5) -> return HiActionNow
      (3, 6) -> HiActionRand <$> decode <*> decode
      (2, 7) -> HiActionEcho <$> decode
      _      -> fail "Invalid HiAction encoding"
