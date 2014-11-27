{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Graphics.UI.MainWindow
    ( createMainWindow
    , setCentralWidget
    , MainWindow()
    , CentralWidgetable() )
    where

import Control.Applicative
import Control.Monad
import Graphics.UI.Internal.Common
import Graphics.UI.MdiArea
import Graphics.UI.Widget

foreign import ccall create_main_window :: IO (Ptr QMainWindow)
foreign import ccall set_central_widget :: Ptr QMainWindow
                                        -> Ptr QWidget
                                        -> IO ()
foreign import ccall take_central_widget :: Ptr QMainWindow
                                         -> IO (Ptr QWidget)
foreign import ccall show_widget :: Ptr QWidget -> IO ()

newtype MainWindow s = MainWindow (ManagedQObject QMainWindow)
                       deriving ( Eq, Typeable
                                , HasQObject
                                , Touchable )

instance HasManagedQObject (MainWindow s) QMainWindow where
    getManagedQObject (MainWindow man) = man

instance Titleable (MainWindow s) s where
    getWidget = coerceManagedQObject . getManagedQObject

--instance HasCommonQObject (MainWindow s) s QMainWindow where
--    getCommonQObject (MainWindow cobject) = cobject

--instance UIElement (MainWindow s) s where
--    delete = deleteCommonQObject . getCommonQObject

-- | Creates a main window and shows it.
--
-- Most applications have one of these but you are allowed to make more than
-- one. Deleting all main windows causes the event loop (and thus
-- `Graphics.UI.runUI`) to return.
--
-- You do not need to hold a reference to the returned `MainWindow` to keep the
-- window alive.
createMainWindow :: UIAction s (MainWindow s)
createMainWindow = liftIO $ mask_ $ do
    w <- create_main_window
    mwindow <- MainWindow <$> (manageQObject =<< createRoot w)
    unsafeUnwrapUIAction $ do
        mdi <- createMdiArea
        setCentralWidget mdi mwindow
    show_widget (castPtr w)
    return mwindow

-- | Sets a central widget to a main window.
--
-- The widget becomes a child of the window and is deleted if the main window
-- is deleted.
setCentralWidget :: CentralWidgetable a s b => a -> MainWindow s -> UIAction s ()
setCentralWidget central_object mwindow = mask_ $ liftIO $
    withManagedQObject mwindow $ \mainwindow_ptr ->
        withManagedQObject central_object $ \child_tr -> do
            void $ take_central_widget mainwindow_ptr
            set_central_widget mainwindow_ptr (castPtr child_tr)

