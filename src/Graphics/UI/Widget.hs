-- | Operations that are common to all `UIElement`s.
--

{-# LANGUAGE ViewPatterns #-}

module Graphics.UI.Widget
    ( title )
    where

import Graphics.UI.Internal.Common
import Graphics.UI.UIVar

foreign import ccall set_widget_title :: Ptr QWidget -> Ptr QString -> IO ()
foreign import ccall get_widget_title :: Ptr QWidget -> IO (Ptr QString)

-- | Sets and gets the title of an `UIElement`.
title :: UIElement a s => a -> UIVar' s Text
title (qwidget -> widget) = uivar
    (\new_value -> liftIO $
        withCommonQObject widget $ \widget_ptr ->
        asQString new_value $ \qstring -> set_widget_title widget_ptr qstring)
    (liftIO $ mask_ $ withCommonQObject widget $ \widget_ptr -> do
        qstring <- get_widget_title widget_ptr
        ret <- peekQString qstring
        freeQString qstring
        return ret)

