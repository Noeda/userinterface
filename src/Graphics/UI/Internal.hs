{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AutoDeriveTypeable #-}

module Graphics.UI.Internal where

import Control.Applicative
import Control.Exception ( allowInterrupt )
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.IORef
import Data.Typeable
import Foreign.Ptr
import System.IO.Unsafe

foreign import ccall "wrapper" wrapIO :: IO () -> IO (FunPtr (IO ()))
foreign export ccall freeHaskellFunPtr :: FunPtr a -> IO ()
foreign import ccall application_quit :: IO ()

wrapAndInsulateIO :: IO () -> IO (FunPtr (IO ()))
wrapAndInsulateIO action = wrapIO $ insulateExceptions action

-- holds an uncaught exception thrown inside `insulateExceptions`
pendingException :: IORef (Maybe SomeException)
pendingException = unsafePerformIO $ newIORef Nothing
{-# NOINLINE pendingException #-}

-- this should restore the masked exception state before `runUI` was called.
restoreFunction :: IORef (IO a -> IO a)
restoreFunction = unsafePerformIO $ newIORef id
{-# NOINLINE restoreFunction #-}

-- wrap an IO action to handle exceptions correctly through Qt (we can't throw
-- an exception with C code between so we have to employ trickery).
insulateExceptions :: IO () -> IO ()
insulateExceptions action = mask $ \restore -> do
    restore2 <- readIORef restoreFunction
    result <- try $ restore (restore2 $ allowInterrupt >> action >> allowInterrupt)
    case result of
        Left err -> writeIORef pendingException (Just err) >>
                    application_quit
        Right ok -> return ok

eventHandler :: Event -> IO ()
eventHandler event = do
    c <- readIORef activeCallback
    case c of
        Nothing -> return ()
        Just cb -> cb event

activeCallback :: IORef (Maybe (Event -> IO ()))
activeCallback = unsafePerformIO $ newIORef Nothing
{-# NOINLINE activeCallback #-}

-- | All user interface functions need to be run inside this monad.
--
-- This monad enforces:
--
--     * All UI functions are run in the same thread.
--     * UI resources cannot be transferred to another UI instance.
--
newtype UIAction s a = UIAction { unsafeUnwrapUIAction :: IO a }
                       deriving ( Monad
                                , MonadThrow
                                , MonadCatch
                                , MonadMask
                                , Functor
                                , MonadFix
                                , Typeable
                                , Applicative )

instance MonadIO (UIAction s) where
    liftIO = UIAction

-- | Global events.
data Event = Ready         -- ^ UI is initialized.
           | AboutToQuit   -- ^ UI is about to close.
                           --   On some platforms, `runUI` may not return so
                           --   you get this event instead.
             deriving ( Eq, Ord, Enum, Read, Show, Typeable )

