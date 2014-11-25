{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | MDI area widget. MDI applications have their own \"desktop\" that in turn
-- has smaller windows.
--

module Graphics.UI.MdiArea
    ( createMdiArea
    , addSubWidget
    , MdiArea()
    , CentralWidgetable() )
    where

import Control.Monad
import Graphics.UI.Internal.Common

foreign import ccall create_mdiarea :: IO (Ptr QMdiArea)
foreign import ccall add_subwindow ::
    Ptr QMdiArea -> Ptr QWidget -> FunPtr (IO ()) -> IO (Ptr QMdiSubWindow)

newtype MdiArea s = MdiArea { mdiAreaPtr :: CommonQObject s QMdiArea }
                    deriving ( Eq, Typeable )

newtype MdiSubWindow s = MdiSubWindow { mdiSubWindowPtr :: CommonQObject s QMdiSubWindow }
                         deriving ( Eq, Typeable )

instance HasCommonQObject (MdiSubWindow s) s QMdiSubWindow where
    getCommonQObject = mdiSubWindowPtr

instance UIElement (MdiSubWindow s) s where
    delete = deleteCommonQObject . getCommonQObject
    qwidget = castCommonQObject . getCommonQObject

instance HasCommonQObject (MdiArea s) s QMdiArea where
    getCommonQObject = mdiAreaPtr

instance UIElement (MdiArea s) s where
    delete = deleteCommonQObject . getCommonQObject
    qwidget = castCommonQObject . getCommonQObject

instance CentralWidgetable (MdiArea s) s QMdiArea where
    centralWidgetableProof _ = ()

-- | Creates an MDI area widget.
--
-- You probably want to use it as a central widget to some main window
-- (`Graphics.UI.MainWindow.setCentralWidget`).
createMdiArea :: UIAction s (MdiArea s)
createMdiArea = liftIO $ mask_ $ do
    mdi_area <- create_mdiarea
    MdiArea <$> addCommonQObject mdi_area Nothing "QMdiArea"

-- | Adds a subwidget to the MDI area.
--
-- The subwidget is deleted if the MDI area is deleted or if the user clicks on
-- the close button.
--
-- Consequences are undefined if you attempt to add the subwidget to another UI
-- construct after this.
addSubWidget :: forall a s b. (UIElement a s, CentralWidgetable a s b)
             => a
             -> MdiArea s
             -> UIAction s ()
addSubWidget widget mdi = liftIO $ mask_ $
    withCommonQObject mdi $ \mdi_ptr ->
        withCommonQObject widget $ \widget_ptr -> do
            let wptr = castPtr widget_ptr
            ac <- wrapAndInsulateIO $ unsafeUnwrapUIAction when_being_deleted
            void $ add_subwindow mdi_ptr wptr ac

            addChild mdi_ptr wptr
                    (parentKeepsAlive $ castCommonQObject $ getCommonQObject widget)
  where
    when_being_deleted :: UIAction s ()
    when_being_deleted = liftIO $ delete widget

