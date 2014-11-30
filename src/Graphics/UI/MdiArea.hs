{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | MDI area widget. MDI applications have their own \"desktop\" that in turn
-- has smaller windows.
--

module Graphics.UI.MdiArea
    ( createMdiArea
    , addSubWidget
    , MdiArea()
    , CentralWidgetable() )
    where

import Graphics.UI.Internal.Common
import Graphics.UI.Widget

foreign import ccall create_mdiarea :: IO (Ptr QMdiArea)
foreign import ccall add_subwindow ::
    Ptr QMdiArea -> Ptr QWidget -> IO (Ptr QMdiSubWindow)

newtype MdiArea = MdiArea (ManagedQObject QMdiArea)
                  deriving ( Eq, Typeable, HasQObject, Touchable )

newtype MdiSubWindow = MdiSubWindow (ManagedQObject QMdiSubWindow)
                       deriving ( Eq, Typeable, HasQObject, Touchable )

instance HasManagedQObject MdiArea QMdiArea where
    getManagedQObject (MdiArea man) = man

instance HasManagedQObject MdiSubWindow QMdiSubWindow where
    getManagedQObject (MdiSubWindow man) = man

instance CentralWidgetable MdiArea

instance Titleable MdiSubWindow

instance IsWidget MdiArea where
    getWidget = coerceManagedQObject . getManagedQObject

instance IsWidget MdiSubWindow where
    getWidget = coerceManagedQObject . getManagedQObject

-- | Creates an MDI area widget.
--
-- You probably want to use it as a central widget to some main window
-- (`Graphics.UI.MainWindow.setCentralWidget`).
createMdiArea :: UIAction MdiArea
createMdiArea = liftIO $ mask_ $ do
    mdi_area <- create_mdiarea
    MdiArea <$> (manageQObject =<< createTrackedQObject mdi_area)

-- | Adds a subwidget to the MDI area.
--
-- The subwidget is deleted if the MDI area is deleted or if the user clicks on
-- the close button.
--
-- Consequences are undefined if you attempt to add the subwidget to another UI
-- construct after this.
addSubWidget :: (IsWidget a, CentralWidgetable a)
             => a
             -> MdiArea
             -> UIAction MdiSubWindow
addSubWidget (getWidget -> widget) mdi = liftIO $ mask_ $
    withManagedQObject widget $ \widget_ptr ->
    withManagedQObject mdi $ \mdi_ptr -> do
        let wptr = castPtr widget_ptr
        MdiSubWindow <$> (manageQObject =<< createTrackedQObject =<<
                          add_subwindow mdi_ptr wptr)

