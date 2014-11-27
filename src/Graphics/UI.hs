{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | This module can initialize UI system and start the event loop.
--

module Graphics.UI
    (
    -- * Running a user interface
      runUI
    , stopUI
    , UIAction()
    -- * Global events
    , Event(..)
    -- * Deleting UI elements promptly
    , Deleteable(..) )
    where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.UI.Internal
import Graphics.UI.Internal.QObject ( garbageCollectionCycle, Deleteable(..) )
import System.Environment
import System.IO.Unsafe

foreign import ccall safe run_qt
    :: CInt -> Ptr (Ptr CChar) -> FunPtr (IO ()) -> IO ()

--foreign export ccall eventCallback :: Ptr QObject -> Ptr QEvent -> IO CInt
foreign export ccall aboutToQuitMsg :: IO ()
foreign export ccall readyMsg :: IO ()

qtActive :: IORef Bool
qtActive = unsafePerformIO $ newIORef False
{-# NOINLINE qtActive #-}

-- | Initializes the user interface and starts the event loop. There can only
-- be one UI instance per process.
--
-- You should use the `Ready` event you will receive in the event handler to
-- initialize whatever user interface you want.
--
-- If you interrupt `runUI` with an exception (asynchronous or just ordinary
-- synchronous exception that you don't catch) the whole UI will be shut down.
--
-- Asynchronous exceptions are unlikely to get through easily to the thread
-- that is running `runUI`. If you need to stop the UI, use `stopUI` which will
-- work from any thread.
runUI :: MonadIO m
      => (forall s. Event -> UIAction s ()) -- ^ Receives global events.
      -> m ()
runUI callback' = liftIO $ runInBoundThread $ mask $ \restore -> do
    ok <- atomicModifyIORef' qtActive $ \old ->
        if old
          then ( old, False )
          else ( True, True )
    unless ok $ error "runUI: Qt5 already active in this process."

    flip finally (writeIORef qtActive False >>
                  writeIORef activeCallback Nothing >>
                  writeIORef restoreFunction id) $ do
        writeIORef activeCallback (Just callback)
        writeIORef pendingException Nothing
        writeIORef restoreFunction restore
        args <- getArgs
        prog <- getProgName
        let params = args ++ [prog]
        withStringArray params $ \arr -> do
            gc <- wrapIO $ insulateExceptions garbageCollectionCycle
            run_qt (fromIntegral $ length params)
                   arr
                   gc
        readIORef pendingException >>= \case
            Just exc -> writeIORef pendingException Nothing >> throwM exc
            Nothing -> return ()
  where
    callback = fmap (insulateExceptions . unsafeUnwrapUIAction) callback'

-- | Stops `runUI`, whatever thread it may be running in. Does nothing if there
-- is no UI running.
stopUI :: IO ()
stopUI = application_quit

-- | Deletes an element. See `UIElement`.
{-deleteUIElement :: UIElement a s => a -> UIAction s ()
deleteUIElement thing = UIAction $ delete thing-}

withStringArray :: [String] -> (Ptr (Ptr CChar) -> IO a) -> IO a
withStringArray strings action =
    recursive strings []
  where
    recursive [] pointers = withArray (reverse pointers) action
    recursive (str:rest) pointers =
        withCString str $ \cstr -> recursive rest (cstr:pointers)

aboutToQuitMsg :: IO ()
aboutToQuitMsg = eventHandler AboutToQuit

readyMsg :: IO ()
readyMsg = eventHandler Ready

