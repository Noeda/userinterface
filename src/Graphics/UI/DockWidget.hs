{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Dockable widgets.
--

module Graphics.UI.DockWidget
    ( createDockWidget
    , DockWidget() )
    where

import Graphics.UI.Internal.Common
import Graphics.UI.MainWindow
import Graphics.UI.Widget

foreign import ccall create_dockwidget
    :: Ptr QMainWindow
    -> Ptr QWidget
    -> FunPtr (IO ())
    -> IO (Ptr QDockWidget)

newtype DockWidget = DockWidget (ManagedQObject QDockWidget)
                     deriving ( Eq, Typeable, HasQObject, Touchable )

instance HasManagedQObject DockWidget QDockWidget where
    getManagedQObject (DockWidget man) = man

instance Titleable DockWidget

instance IsWidget DockWidget where
    getWidget = coerceManagedQObject . getManagedQObject

-- | Creates a dock widget and attaches it to a main window.
--
-- Dock widget needs something inside it. Dock widget takes ownership of the
-- child widget.
--
-- Consequences are undefined if you attempt to add the subwidget to another UI
-- construct after this.
createDockWidget :: (IsWidget a, CentralWidgetable a)
                 => MainWindow
                 -> a   -- ^ What to show inside the dock widget.
                 -> UIAction DockWidget
createDockWidget mainwindow (getWidget -> child_widget) = liftIO $ mask_ $ do
    withManagedQObject mainwindow $ \mainwindow_ptr -> do
        withManagedQObject child_widget $ \child_ptr -> do
            ac <- wrapAndInsulateIO $ return ()
            dockwidget_ptr <- create_dockwidget mainwindow_ptr (castPtr child_ptr) ac
            dockwidget_cobject <- manageQObject =<< createTrackedQObject dockwidget_ptr
            return $ DockWidget dockwidget_cobject

