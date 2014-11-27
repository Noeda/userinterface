-- | Operations that are common to all `UIElement`s.
--

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Graphics.UI.Widget
    ( title
    , Titleable(..) )
    where

import Graphics.UI.Internal.Common
import Graphics.UI.UIVar

foreign import ccall set_widget_title :: Ptr QWidget -> Ptr QString -> IO ()
foreign import ccall get_widget_title :: Ptr QWidget -> IO (Ptr QString)

class Titleable a s | a -> s where
    getWidget :: a -> ManagedQObject QWidget

-- | Sets and gets the title of a widget.
title :: Titleable a s => a -> UIVar' s Text
title (getWidget -> widget) = uivar
    (\new_value -> liftIO $
        withManagedQObject widget $ \widget_ptr ->
        asQString new_value $ \qstring -> set_widget_title widget_ptr qstring)
    (liftIO $ mask_ $ withManagedQObject widget $ \widget_ptr -> do
        qstring <- get_widget_title widget_ptr
        ret <- peekQString qstring
        freeQString qstring
        return ret)

