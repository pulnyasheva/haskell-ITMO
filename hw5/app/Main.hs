module Main (main) where

import Control.Monad.IO.Class
import Data.Set
import HW5.Action (HIO (..), HiPermission (..))
import HW5.Evaluator (eval)
import HW5.Parser (parse)
import HW5.Pretty (prettyValue)
import System.Console.Haskeline
import Text.Megaparsec.Error (errorBundlePretty)


main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      input <- getInputLine "hi> "
      case input of
        Nothing -> return ()
        Just line -> do
          case parse line of
            Left err -> outputStrLn (errorBundlePretty err)
            Right expr -> do
              result <- liftIO $ runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime])
              case result of
                Left err  -> outputStrLn (show err)
                Right val -> outputStrLn (show (prettyValue val))
      loop
