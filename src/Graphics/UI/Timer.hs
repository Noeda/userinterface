{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Concurrent
import Graphics.UI.Internal.Common

foreign import ccall create_timer :: CInt
                                  -> CInt
                                  -> FunPtr (IO ())
                                  -> IO (Ptr QTimer)

type Milliseconds = Int

newtype Timer = Timer (ManagedQObject QTimer)
                deriving ( Eq, Typeable, HasQObject, Touchable )

instance HasManagedQObject Timer QTimer where
    getManagedQObject (Timer man) = man

-- | Schedule a `UIAction` to run after given number of milliseconds.
--
-- Timers won't be run if the event loop is not running. That is, after
-- `Graphics.UI.runUI` has returned, no timer actions will be run.
createTimer :: Bool                   -- ^ One-shot? If `True`, the timer runs
                                      --   once and is then deleted. Otherwise
                                      --   the action will be run again and
                                      --   again every N milliseconds.
            -> Milliseconds
            -> UIAction ()
            -> UIAction Timer
createTimer oneshot milliseconds (UIAction (insulateExceptions -> action)) = liftIO $ mask_ $ do
    mvar <- newEmptyMVar
    wrapped_action <- wrapIO $ do
        tvar <- readMVar mvar
        if oneshot
          then (finally action $ unRoot tvar)
          else action
    timer_ptr <-
         create_timer (if oneshot then 1 else 0)
                      (max 0 $ fromIntegral $
                       min milliseconds (fromIntegral (maxBound :: CInt)))
                      wrapped_action
    timer <- Timer <$> (manageQObject =<< createRoot timer_ptr)
    putMVar mvar $ getQObject $ getManagedQObject timer
    return timer

