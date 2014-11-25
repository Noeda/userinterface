{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Timers.

module Graphics.UI.Timer
    ( createTimer
    , Milliseconds )
    where

import Control.Applicative
import Graphics.UI.Internal.Common

foreign import ccall create_timer :: CInt
                                  -> CInt
                                  -> FunPtr (IO ())
                                  -> IO (Ptr QTimer)

type Milliseconds = Int

newtype Timer s = Timer { timerPtr :: CommonQObject s QTimer }

instance HasCommonQObject (Timer s) s QTimer where
    getCommonQObject = timerPtr

instance UIElement (Timer s) s where
    delete = deleteCommonQObject . getCommonQObject

-- | Schedule a `UIAction` to run after given number of milliseconds.
--
-- Timers won't be run if the event loop is not running. That is, after
-- `Graphics.UI.runUI` has returned, no timer actions will be run.
createTimer :: Bool                   -- ^ One-shot? If `True`, the timer runs
                                      --   once and is then deleted. Otherwise
                                      --   the action will be run again and
                                      --   again every N milliseconds.
            -> Milliseconds
            -> UIAction s ()
            -> UIAction s (Timer s)
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
                  "QTimer"


