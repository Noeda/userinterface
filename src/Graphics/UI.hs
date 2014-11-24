{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.UI
    ( runUI
    , deleteUIElement
    , Event(..)
    , UIAction() )
    where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Set as S
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import GHC.IO ( unsafeUnmask )
import Graphics.UI.Internal
import System.Environment
import System.IO.Unsafe

data QObject
data QEvent

foreign import ccall safe run_qt :: CInt -> Ptr (Ptr CChar) -> IO ()

--foreign export ccall eventCallback :: Ptr QObject -> Ptr QEvent -> IO CInt
foreign export ccall aboutToQuitMsg :: IO ()
foreign export ccall readyMsg :: IO ()

qtActive :: IORef Bool
qtActive = unsafePerformIO $ newIORef False
{-# NOINLINE qtActive #-}

runUI :: MonadIO m => (forall s. Event -> UIAction s ()) -> m ()
runUI callback' = liftIO $ runInBoundThread $ mask $ \restore -> do
    ok <- atomicModifyIORef' qtActive $ \old ->
        if old
          then ( old, False )
          else ( True, True )
    unless ok $ error "runUI: Qt5 already active in this process."

    flip finally (writeIORef qtActive False >>
                  writeIORef activeCallback Nothing >>
                  writeIORef topReferences S.empty) $ do
        writeIORef activeCallback (Just callback)
        args <- getArgs
        prog <- getProgName
        let params = args ++ [prog]
        withStringArray params $ run_qt (fromIntegral $ length params)
  where
    callback = fmap (insulateExceptions . unsafeUnwrapUIAction) callback'

deleteUIElement :: UIElement a => a -> UIAction s ()
deleteUIElement thing = UIAction $ delete thing

withStringArray :: [String] -> (Ptr (Ptr CChar) -> IO a) -> IO a
withStringArray strings action =
    recursive strings []
  where
    num_strings = length strings

    recursive [] pointers = withArray (reverse pointers) action
    recursive (str:rest) pointers =
        withCString str $ \cstr -> recursive rest (cstr:pointers)

aboutToQuitMsg :: IO ()
aboutToQuitMsg = eventHandler AboutToQuit

readyMsg :: IO ()
readyMsg = eventHandler Ready

