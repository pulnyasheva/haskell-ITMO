module HW5.Action
  ( HiPermission(..)
  , PermissionException(..)
  , HIO(..)
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import Data.Set
import Data.Text
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock (getCurrentTime)
import HW5.Base (HiAction (..), HiMonad (..), HiValue (..))
import System.Directory
import System.Random

data HiPermission = AllowRead
                      | AllowWrite
                      | AllowTime
                      deriving (Eq, Ord, Show)

newtype PermissionException = PermissionRequired HiPermission
                            deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap f (HIO x) = HIO (fmap f . x)

instance Applicative HIO where
  pure = return
  m <*> k = m >>= (\x -> k >>= (\y -> return (x y)))

instance Monad HIO where
  return x = HIO (\_ -> return x)
  m >>= k = HIO (\perms -> do
                  x <- runHIO m perms
                  runHIO (k x) perms)

instance HiMonad HIO where
  runAction HiActionCwd                = HIO actionCwd
  runAction (HiActionChDir path)       = HIO $ actionChDir path
  runAction (HiActionRead path)        = HIO $ actionRead path
  runAction (HiActionWrite path bytes) = HIO $ actionWrite path bytes
  runAction (HiActionMkDir path)       = HIO $ actionMkDir path
  runAction HiActionNow                = HIO actionNow
  runAction (HiActionRand x y)         = HIO (\_ -> actionRand x y)
  runAction (HiActionEcho text)        = HIO $ actionEcho text

actionCwd :: Set HiPermission -> IO HiValue
actionCwd permissionsSet = if checkPermissions permissionsSet AllowRead
                           then HiValueString . T.pack <$> getCurrentDirectory
                           else throwIO $ PermissionRequired AllowRead

actionChDir :: FilePath -> Set HiPermission -> IO HiValue
actionChDir path permissionsSet = if checkPermissions permissionsSet AllowRead
                               then do
                                setCurrentDirectory path
                                return HiValueNull
                               else throwIO $ PermissionRequired AllowRead

actionRead :: FilePath -> Set HiPermission -> IO HiValue
actionRead path permissionsSet = if checkPermissions permissionsSet AllowRead
                                 then do
                                    dir <- doesDirectoryExist path
                                    if dir
                                    then do
                                        contents <- listDirectory path
                                        return $ HiValueList $ S.fromList $ Prelude.map (HiValueString . T.pack) contents
                                    else do
                                      file <- doesFileExist path
                                      if file
                                      then do
                                        bytes <- B.readFile path
                                        case decodeUtf8' bytes of
                                          Left _     -> return $ HiValueBytes bytes
                                          Right text -> return $ HiValueString text
                                        else return HiValueNull
                                 else throwIO $ PermissionRequired AllowRead

actionWrite :: FilePath -> ByteString -> Set HiPermission -> IO HiValue
actionWrite path bytes permissionsSet = if checkPermissions permissionsSet AllowWrite
                                        then do
                                          B.writeFile path bytes
                                          return HiValueNull
                                        else throwIO $ PermissionRequired AllowWrite

actionMkDir :: FilePath -> Set HiPermission -> IO HiValue
actionMkDir path permissionsSet = if checkPermissions permissionsSet AllowWrite
                                  then do
                                    createDirectory path
                                    return HiValueNull
                                  else throwIO $ PermissionRequired AllowWrite

actionNow :: Set HiPermission -> IO HiValue
actionNow permissionsSet = if checkPermissions permissionsSet AllowTime
                           then HiValueTime <$> getCurrentTime
                           else throwIO $ PermissionRequired AllowTime

actionRand :: Int -> Int -> IO HiValue
actionRand x y = do
  rand <- randomRIO (x, y)
  return (HiValueNumber (toRational rand))

actionEcho :: Text -> Set HiPermission -> IO HiValue
actionEcho text permissionsSet = if checkPermissions permissionsSet AllowWrite
                                 then do
                                   putStrLn (T.unpack text)
                                   return HiValueNull
                                 else throwIO $ PermissionRequired AllowWrite

checkPermissions :: Set HiPermission -> HiPermission -> Bool
checkPermissions permissionsSet permission = member permission permissionsSet
