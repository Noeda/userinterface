-- | Operations that are common to all `UIElement`s.
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Graphics.UI.Widget
    ( title
    , setFixedSize
    , setLayout
    , layout
    , Size(..)
    , Titleable(..)
    , IsWidget(..)
    , IsLayout(..) )
    where

import Graphics.UI.Internal.Common
import Graphics.UI.UIVar

foreign import ccall set_widget_title :: Ptr QWidget -> Ptr QString -> IO ()
foreign import ccall get_widget_title :: Ptr QWidget -> IO (Ptr QString)
foreign import ccall set_widget_fixed_size
    :: Ptr QWidget -> CInt -> CInt -> IO ()
foreign import ccall set_widget_layout :: Ptr QWidget -> Ptr QLayout -> IO ()
foreign import ccall get_widget_layout :: Ptr QWidget -> IO (Ptr QLayout)

-- | Things that can have a title.
class IsWidget a => Titleable a where
    setTitle :: Proxy a -> Ptr QWidget -> Ptr QString -> IO ()
    setTitle _ = set_widget_title

    getTitle :: Proxy a -> Ptr QWidget -> IO (Ptr QString)
    getTitle _ = get_widget_title

-- | Things that are QWidgets at some level of object hierarchy.
class IsWidget a where
    getWidget :: a -> ManagedQObject QWidget

-- | Things that are QLayouts
class IsLayout a where
    getLayout :: a -> ManagedQObject QLayout

-- | Sets and gets the title of a widget.
title :: forall a. Titleable a => a -> UIVar' Text
title (getWidget -> widget) = uivar
    (\new_value -> liftIO $
        withManagedQObject widget $ \widget_ptr ->
        asQString new_value $ \qstring -> setTitle (Proxy :: Proxy a) widget_ptr qstring)
    (liftIO $ mask_ $ withManagedQObject widget $ \widget_ptr -> do
        qstring <- getTitle (Proxy :: Proxy a) widget_ptr
        ret <- peekQString qstring
        freeQString qstring
        return ret)

data Size = Size { width :: !Int, height :: !Int }
            deriving ( Eq, Ord, Show, Read, Typeable )

-- | Sets a fixed size for a widget. `Nothing` means no fixed size.
setFixedSize :: IsWidget a => Maybe Size -> a -> UIAction ()
setFixedSize size (getWidget -> widget) =
    liftIO $ withManagedQObject widget $ \widget_ptr ->
        case size of
            Nothing -> set_widget_fixed_size widget_ptr (-1) (-1)
            Just (Size (fromIntegral -> w) (fromIntegral -> h)) ->
                set_widget_fixed_size widget_ptr w h

-- | Sets widget layout.
setLayout :: (IsLayout b, IsWidget a) => b -> a -> UIAction ()
setLayout (getLayout -> layout) (getWidget -> widget) = liftIO $
    withManagedQObject widget $ \widget_ptr ->
    withManagedQObject layout $ \layout_ptr ->
        set_widget_layout widget_ptr layout_ptr

newtype SomeLayout = SomeLayout (ManagedQObject QLayout)
                     deriving ( Eq, Typeable, Touchable, HasQObject )

instance HasManagedQObject SomeLayout QLayout where
    getManagedQObject (SomeLayout man) = man

instance IsLayout SomeLayout where
    getLayout (SomeLayout man) = coerceManagedQObject man

-- | Gets widget layout.
layout :: IsWidget a => a -> UIAction (Maybe SomeLayout)
layout (getWidget -> widget) = liftIO $
    withManagedQObject widget $ \widget_ptr -> do
        r <- get_widget_layout widget_ptr
        if r == nullPtr
          then return Nothing
          else Just . SomeLayout <$> (manageQObject =<< createTrackedQObject r)


