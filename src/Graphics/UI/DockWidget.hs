{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Dockable widgets.
--

module Graphics.UI.DockWidget
    ( createDockWidget
    , DockWidget() )
    where

import Control.Concurrent
import Graphics.UI.Internal.Common
import Graphics.UI.MainWindow

foreign import ccall create_dockwidget
    :: Ptr QMainWindow
    -> Ptr QWidget
    -> FunPtr (IO ())
    -> IO (Ptr QDockWidget)

newtype DockWidget s = DockWidget { dockWidgetPtr :: CommonQObject s QDockWidget }
                       deriving ( Eq, Typeable )

instance HasCommonQObject (DockWidget s) s QDockWidget where
    getCommonQObject = dockWidgetPtr

instance UIElement (DockWidget s) s where
    delete = deleteCommonQObject . getCommonQObject
    qwidget = castCommonQObject . getCommonQObject

-- | Creates a dock widget and attaches it to a main window.
--
-- Dock widget needs something inside it. Dock widget takes ownership of the
-- child widget.
--
-- Consequences are undefined if you attempt to add the subwidget to another UI
-- construct after this.
createDockWidget :: forall a s b. (UIElement a s, CentralWidgetable a s b)
                 => MainWindow s
                 -> a   -- ^ What to show inside the dock widget.
                 -> UIAction s (DockWidget s)
createDockWidget mainwindow child_widget = liftIO $ mask_ $ do
    withCommonQObject mainwindow $ \mainwindow_ptr -> do
        withCommonQObject child_widget $ \child_ptr -> do
            mvar <- newEmptyMVar
            ac <- wrapAndInsulateIO $ do
                ob <- takeMVar mvar
                delete ob
            dockwidget_ptr <- create_dockwidget mainwindow_ptr (castPtr child_ptr) ac
            dockwidget_cobject <- addCommonQObject dockwidget_ptr Nothing "QDockWidget"
            let dockwidget = DockWidget dockwidget_cobject
            putMVar mvar dockwidget

            addChild mainwindow_ptr dockwidget_ptr
                     (parentKeepsAlive $
                      castCommonQObject $
                      getCommonQObject dockwidget)
            addChild dockwidget_ptr child_ptr
                     (parentKeepsAlive $
                      castCommonQObject $
                      getCommonQObject child_widget)
            return dockwidget

