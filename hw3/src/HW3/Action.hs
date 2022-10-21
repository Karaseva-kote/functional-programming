module HW3.Action
  ( HiPermission (..)
  , PermissionException (..)
  , HIO (..)
  ) where
  import GHC.Exception.Type
  import Data.Set ( Set, member )
  import HW3.Base

  import System.Directory
  import qualified Control.Monad
  import Control.Exception
  import Data.Text (pack, unpack)
  import Data.Sequence (fromList)
  import qualified Data.ByteString as B
  import Data.Text.Encoding (decodeUtf8, decodeUtf8')
  import Data.Time (getCurrentTime)
  import System.Random
  
  data HiPermission
    = AllowRead
    | AllowWrite
    -- 
    | AllowTime
    deriving (Show, Eq, Ord, Enum, Bounded)

  data PermissionException
    = PermissionRequired HiPermission
    deriving Show

  instance Exception PermissionException

  newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

  joinHIO :: HIO (HIO a) -> HIO a
  joinHIO hio = HIO $ \set -> do
    hio2 <- runHIO hio set
    runHIO hio2 set

  instance Monad HIO where
    (>>=) m f = joinHIO (fmap f m)

  instance Applicative HIO where
    pure a = HIO (\_ -> pure a)
    (<*>) p q = Control.Monad.ap p q

  instance Functor HIO where
    fmap f hio = HIO (fmap f . runHIO hio)

  instance HiMonad HIO where
    runAction (HiActionRead path)      = getHIOHiValue AllowRead $ readFun path
    runAction (HiActionWrite path bs)  = getHIOHiValue AllowWrite $ do
      _ <- writeFile path (unpack (decodeUtf8 bs))
      pure HiValueNull
    runAction (HiActionMkDir path)     = getHIOHiValue AllowWrite $ do
      _ <- createDirectory path
      pure HiValueNull
    runAction (HiActionChDir path)     = getHIOHiValue AllowRead $ do
      _ <- setCurrentDirectory path
      pure HiValueNull
    runAction HiActionCwd              = getHIOHiValue AllowRead $ do
      HiValueString . pack <$> getCurrentDirectory
    runAction HiActionNow              = getHIOHiValue AllowTime $ do
      HiValueTime <$> getCurrentTime
    runAction (HiActionRand a b)       = HIO $ \_ -> do
      r <- getStdRandom (randomR (a, b))
      pure $ HiValueNumber $ toRational r
    runAction (HiActionEcho t)         = getHIOHiValue AllowWrite $ do
       _ <- putStrLn (unpack t)
       pure HiValueNull

  readFun :: FilePath -> IO HiValue
  readFun path = do
    exist <- doesDirectoryExist path
    if exist
      then do
        list <- listDirectory path
        pure $ HiValueList $ fromList $ map (HiValueString . pack) list
      else do
        byte <- B.readFile path
        case decodeUtf8' byte of
          (Right t) -> pure $ HiValueString t
          (Left _)  -> pure $ HiValueBytes byte

  getHIOHiValue :: HiPermission -> IO HiValue -> HIO HiValue
  getHIOHiValue perm io = HIO $ \set ->
    if member perm set
      then io
      else throwIO (PermissionRequired perm)