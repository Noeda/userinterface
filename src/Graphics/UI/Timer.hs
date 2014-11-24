{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE ViewPatterns #-}

module Graphics.UI.Timer
    ( createTimer
    , Milliseconds )
    where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Internal
import Graphics.UI.Internal.Touchable
import Graphics.UI.Internal.QObject
import Graphics.UI.Internal.QTypes
import Foreign.C.Types
import Foreign.Ptr

foreign import ccall create_timer :: CInt
                                  -> CInt
                                  -> FunPtr (IO ())
                                  -> IO (Ptr QTimer)
foreign import ccall "wrapper" wrapIO :: IO () -> IO (FunPtr (IO ()))

type Milliseconds = Int

newtype Timer s = Timer { timerPtr :: CommonQObject QTimer }

instance HasCommonQObject (Timer s) QTimer where
    getCommonQObject = timerPtr

instance UIElement (Timer s) where
    delete = deleteCommonQObject . getCommonQObject

createTimer :: Bool -> Milliseconds -> UIAction s () -> UIAction s (Timer s)
createTimer oneshot milliseconds (UIAction (insulateExceptions -> action)) = liftIO $ mask_ $ do
    -- this IORef magic keeps the timer alive until it shoots
    -- [1] Keep the IORef that we need to touch in toucher_ref
    dummy_ref <- newIORef ()
    toucher_ref <- newIORef dummy_ref
    wrapped_action <- wrapIO (if oneshot
                               then (finally action $ writeIORef toucher_ref dummy_ref)
                               else action)
    timer <- create_timer (if oneshot then 1 else 0)
                          (max 0 $ fromIntegral $
                            min milliseconds (fromIntegral (maxBound :: CInt)))
                          wrapped_action
    Timer <$> addCommonQObject timer (Just $ \ref -> do
                  writeIORef toucher_ref ref -- [2] Touch the finalizer
                  return $ readIORef toucher_ref >>= touch)

