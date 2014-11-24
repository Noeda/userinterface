{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AutoDeriveTypeable #-}

module Graphics.UI.Internal where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Set as S
import Data.Typeable
import Graphics.UI.Internal.QTypes
import Foreign.ForeignPtr
import Foreign.Ptr
import System.Exit
import System.IO
import System.IO.Unsafe

insulateExceptions :: IO a -> IO a
insulateExceptions action = mask $ \restore -> do
    result <- try $ restore action
    case result of
        Left err -> do
            hPutStrLn stderr $ "insulateExceptions: Uncaught exception in a Qt callback. We cannot continue. " ++ show (err :: SomeException)
            exitFailure
        Right ok -> return ok

class UIElement a where
    delete :: a -> IO ()

eventHandler :: Event -> IO ()
eventHandler event = do
    c <- readIORef activeCallback
    case c of
        Nothing -> return ()
        Just cb -> cb event

activeCallback :: IORef (Maybe (Event -> IO ()))
activeCallback = unsafePerformIO $ newIORef Nothing
{-# NOINLINE activeCallback #-}

topReferences :: IORef (S.Set (ForeignPtr ()))
topReferences = unsafePerformIO $ newIORef S.empty
{-# NOINLINE topReferences #-}

addTopForeignPtr :: ForeignPtr a -> IO ()
addTopForeignPtr fptr = atomicModifyIORef' topReferences $ \old ->
    ( S.insert (castForeignPtr fptr) old, () )

newtype UIAction s a = UIAction { unsafeUnwrapUIAction :: IO a }
                       deriving ( Monad
                                , Functor
                                , MonadFix
                                , Typeable
                                , Applicative )

instance MonadIO (UIAction s) where
    liftIO = UIAction

data Event = Ready
           | AboutToQuit
             deriving ( Eq, Ord, Enum, Read, Show, Typeable )

