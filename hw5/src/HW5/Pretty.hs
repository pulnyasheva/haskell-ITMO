module HW5.Pretty
  ( prettyValue
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ()
import Data.Ratio (denominator, numerator)
import Data.Scientific (fromRationalRepetendUnlimited)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word8)
import HW5.Base (HiAction (..), HiFun (..), HiValue (..))
import Prettyprinter (Doc, brackets, colon, comma, hcat, lbrace, parens, pretty, punctuate, rbrace,
                      space)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Printf


prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber rt)     = prettyNumber rt
prettyValue (HiValueFunction fun)  = prettyFun fun
prettyValue (HiValueBool bool)     = prettyBool bool
prettyValue HiValueNull            = prettyNull
prettyValue (HiValueString text)   = prettyString text
prettyValue (HiValueList list)     = prettyList list
prettyValue (HiValueBytes bytes)   = prettyBytes bytes
prettyValue (HiValueAction action) = prettyAction action
prettyValue (HiValueTime time)     = prettyTime time
prettyValue (HiValueDict dict)     = prettyDict dict

prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber rt = case quotRem (numerator rt) (denominator rt) of
  (q, 0) -> pretty (show q)
  (q, r) -> case fromRationalRepetendUnlimited rt of
    (n, Nothing) -> pretty (show n)
    _ -> let d = denominator rt
         in if q >= 0
            then if q == 0
                 then pretty (show r ++ "/" ++ show d)
                 else pretty (show q ++ " + " ++ show r ++ "/" ++ show d)
            else pretty (show q ++ " - " ++ show (abs r) ++ "/" ++ show d)

prettyFun :: HiFun -> Doc AnsiStyle
prettyFun fun = case fun of
  HiFunAdd            -> pretty "add"
  HiFunSub            -> pretty "sub"
  HiFunMul            -> pretty "mul"
  HiFunDiv            -> pretty "div"
  HiFunNot            -> pretty "not"
  HiFunAnd            -> pretty "and"
  HiFunOr             -> pretty "or"
  HiFunLessThan       -> pretty "less-than"
  HiFunGreaterThan    -> pretty "greater-than"
  HiFunEquals         -> pretty "equals"
  HiFunNotLessThan    -> pretty "not-less-than"
  HiFunNotGreaterThan -> pretty "not-greater-than"
  HiFunNotEquals      -> pretty "not-equals"
  HiFunIf             -> pretty "if"
  HiFunLength         -> pretty "length"
  HiFunToUpper        -> pretty "to-upper"
  HiFunToLower        -> pretty "to-lower"
  HiFunReverse        -> pretty "reverse"
  HiFunTrim           -> pretty "trim"
  HiFunList           -> pretty "list"
  HiFunRange          -> pretty "range"
  HiFunFold           -> pretty "fold"
  HiFunPackBytes      -> pretty "pack-bytes"
  HiFunUnpackBytes    -> pretty "unpack-bytes"
  HiFunEncodeUtf8     -> pretty "encode-utf8"
  HiFunDecodeUtf8     -> pretty "decode-utf8"
  HiFunZip            -> pretty "zip"
  HiFunUnzip          -> pretty "unzip"
  HiFunSerialise      -> pretty "serialise"
  HiFunDeserialise    -> pretty "deserialise"
  HiFunRead           -> pretty "read"
  HiFunWrite          -> pretty "write"
  HiFunMkDir          -> pretty "mkdir"
  HiFunChDir          -> pretty "cd"
  HiFunParseTime      -> pretty "parse-time"
  HiFunRand           -> pretty "rand"
  HiFunEcho           -> pretty "echo"
  HiFunCount          -> pretty "count"
  HiFunKeys           -> pretty "keys"
  HiFunValues         -> pretty "values"
  HiFunInvert         -> pretty "invert"

prettyBool :: Bool -> Doc AnsiStyle
prettyBool True  = pretty "true"
prettyBool False = pretty "false"

prettyNull :: Doc AnsiStyle
prettyNull = pretty "null"

prettyString :: Text -> Doc AnsiStyle
prettyString text = pretty (Text.pack "\"" <> text <> Text.pack "\"")

prettyStick :: Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyStick sep doc = hcat $ punctuate sep doc

prettyList :: Seq HiValue -> Doc AnsiStyle
prettyList list = brackets $ prettyStick (comma <> space) $ map prettyValue (toList list)

prettyBytesWithSpace :: ByteString -> Doc AnsiStyle
prettyBytesWithSpace bytes = prettyStick space $ map prettyByte (ByteString.unpack bytes)

prettyByte :: Word8 -> Doc AnsiStyle
prettyByte = pretty . (printf "%02x" :: Word8 -> String)

prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes bytes = brackets (pretty "#" <> space <> prettyBytesWithSpace bytes <> space <> pretty "#")

prettyFunWithArg :: HiFun -> [HiValue] -> Doc AnsiStyle
prettyFunWithArg fun list = prettyFun fun <> parens (prettyStick (comma <> space) $ map prettyValue list)

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction (HiActionRead path) = prettyFunWithArg HiFunRead [HiValueString (Text.pack path)]
prettyAction (HiActionWrite path bytes) = prettyFunWithArg HiFunWrite [HiValueString (Text.pack path), HiValueBytes bytes]
prettyAction (HiActionMkDir path) = prettyFunWithArg HiFunMkDir [HiValueString (Text.pack path)]
prettyAction (HiActionChDir path) = prettyFunWithArg HiFunChDir [HiValueString (Text.pack path)]
prettyAction HiActionCwd = pretty "cwd"
prettyAction HiActionNow = pretty "now"
prettyAction (HiActionRand x y) = pretty "rand" <> parens (space <> pretty x <> comma <> space <> pretty y <> space)
prettyAction (HiActionEcho text) = prettyFunWithArg HiFunEcho [HiValueString text]

textFormat :: UTCTime -> Text
textFormat time = Text.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" time)

prettyTime :: UTCTime -> Doc AnsiStyle
prettyTime time = prettyFunWithArg HiFunParseTime [HiValueString $ textFormat time]

prettyDict :: Map HiValue HiValue -> Doc AnsiStyle
prettyDict dict = lbrace <> space <> prettyStick (comma <> space) pairs <> space <> rbrace
               where
                 pairs = map pairToDoc $ Map.toList dict
                 pairToDoc (k, v) = prettyValue k <> colon <> space <> prettyValue v

