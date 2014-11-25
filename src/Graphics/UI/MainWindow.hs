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
import Graphics.UI.Internal.Common
import Graphics.UI.MdiArea

foreign import ccall create_main_window :: IO (Ptr QMainWindow)
foreign import ccall set_central_widget :: Ptr QMainWindow
                                        -> Ptr QWidget
                                        -> IO ()
foreign import ccall take_central_widget :: Ptr QMainWindow
                                         -> IO (Ptr QWidget)
foreign import ccall show_widget :: Ptr QWidget -> IO ()

newtype MainWindow s = MainWindow (CommonQObject s QMainWindow)
                       deriving ( Eq, Typeable )

instance Touchable (MainWindow s) where
    touch (MainWindow cqo) = touch cqo

instance HasCommonQObject (MainWindow s) s QMainWindow where
    getCommonQObject (MainWindow cobject) = cobject

instance UIElement (MainWindow s) s where
    delete = deleteCommonQObject . getCommonQObject
    qwidget = castCommonQObject . getCommonQObject

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
    ret <- MainWindow <$> addCommonQObject w staysAliveByItself "QMainWindow"
    unsafeUnwrapUIAction $ do
        mdi <- createMdiArea
        setCentralWidget mdi ret
    show_widget (castPtr w)
    return ret

-- | Sets a central widget to a main window.
--
-- The widget becomes a child of the window and is deleted if the main window
-- is deleted.
setCentralWidget :: CentralWidgetable a s b => a -> MainWindow s -> UIAction s ()
setCentralWidget (getCommonQObject -> central_cobject)
                 (MainWindow cobject) = liftIO $ mask_ $
    withCommonQObject cobject $ \mainwindow_ptr ->
        withCommonQObject central_cobject $ \child_tr -> do
            taken_qobject <- take_central_widget mainwindow_ptr
            set_central_widget mainwindow_ptr (castPtr child_tr)
            removeChildIfExists mainwindow_ptr taken_qobject
            addChild mainwindow_ptr child_tr (parentKeepsAlive central_cobject)

