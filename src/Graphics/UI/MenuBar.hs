{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

-- | Menu bar and menus.
--

module Graphics.UI.MenuBar
    ( createMenuBar
    , createMenu
    , addMenuAction
    , MenuBar()
    , Menu()
    , MenuParent() )
    where

import qualified Data.Text as T
import Graphics.UI.Internal.Common
import Graphics.UI.MainWindow
import Graphics.UI.Widget

foreign import ccall create_menubar :: IO (Ptr QMenuBar)
foreign import ccall add_menubar_menu :: Ptr QMenuBar -> Ptr QString -> IO (Ptr QMenu)
foreign import ccall add_menu_menu :: Ptr QMenu -> Ptr QString -> IO (Ptr QMenu)
foreign import ccall set_menubar :: Ptr QMainWindow -> Ptr QMenuBar -> IO ()
foreign import ccall add_menu_action ::
    Ptr QMenu -> Ptr QString -> FunPtr (IO ()) -> IO ()

newtype MenuBar = MenuBar (ManagedQObject QMenuBar)
                  deriving ( Eq, Typeable, HasQObject, Touchable )

newtype Menu = Menu (ManagedQObject QMenu)
               deriving ( Eq, Typeable, HasQObject, Touchable )

instance IsWidget MenuBar where
    getWidget = coerceManagedQObject . getManagedQObject

instance IsWidget Menu where
    getWidget = coerceManagedQObject . getManagedQObject

instance HasManagedQObject MenuBar QMenuBar where
    getManagedQObject (MenuBar man) = man

instance HasManagedQObject Menu QMenu where
    getManagedQObject (Menu man) = man

class HasManagedQObject a b => MenuParent a b | a -> b where
    addMenu :: Proxy a -> Ptr QWidget -> Ptr QString -> IO (Ptr QMenu)

instance MenuParent Menu QMenu where
    addMenu _ widget menu = add_menu_menu (castPtr widget) menu

instance MenuParent MenuBar QMenuBar where
    addMenu _ widget menu = add_menubar_menu (castPtr widget) menu

-- | Creates a menu bar.
createMenuBar :: MainWindow -> UIAction MenuBar
createMenuBar mainwindow = liftIO $ mask_ $
    withManagedQObject mainwindow $ \window_ptr -> do
        mbar <- create_menubar
        set_menubar window_ptr mbar
        MenuBar <$> (manageQObject =<< createTrackedQObject mbar)

-- | Creates a menu and puts it in a menu bar.
createMenu :: forall a b. MenuParent a b => T.Text -> a -> UIAction Menu
createMenu title_text parent_widget = liftIO $ do
    asQString title_text $ \qstring -> withManagedQObject parent_widget $ \parent -> do
        menu <- addMenu (Proxy :: Proxy a) (castPtr parent) qstring
        Menu <$> (manageQObject =<< createTrackedQObject menu)

-- | Adds an action to a menu.
addMenuAction :: T.Text           -- ^ The text on the action.
              -> Menu             -- ^ Where to add the action.
              -> UIAction ()      -- ^ Called when the action is selected.
              -> UIAction ()
addMenuAction title_text menu (UIAction (insulateExceptions -> action)) = liftIO $ mask_ $
    asQString title_text $ \qstring -> do
        withManagedQObject menu $ \ptr -> do
            wrapped <- wrapIO action  -- cleaned up by Qt
            add_menu_action ptr qstring wrapped

