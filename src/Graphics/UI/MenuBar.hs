{-# LANGUAGE MultiParamTypeClasses #-}
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

foreign import ccall create_menubar :: IO (Ptr QMenuBar)
foreign import ccall create_menu :: Ptr QString -> IO (Ptr QMenu)
foreign import ccall add_menubar_menu :: Ptr QMenuBar -> Ptr QMenu -> IO ()
foreign import ccall set_menubar :: Ptr QMainWindow -> Ptr QMenuBar -> IO ()
foreign import ccall add_menu_action ::
    Ptr QMenu -> Ptr QString -> FunPtr (IO ()) -> IO ()

data MenuBar s = MenuBar { menuBarPtr :: CommonQObject s QMenuBar
                         , mainWindow :: MainWindow s }
                 deriving ( Eq, Typeable )

instance HasCommonQObject (MenuBar s) s QMenuBar where
    getCommonQObject = menuBarPtr

instance UIElement (MenuBar s) s where
    delete = deleteCommonQObject . getCommonQObject
    qwidget = castCommonQObject . getCommonQObject

instance UIElement (Menu s) s where
    delete = deleteCommonQObject . getCommonQObject
    qwidget = castCommonQObject . getCommonQObject

data Menu s where
    Menu :: MenuParent a s => CommonQObject s QMenu -> a -> Menu s

instance HasCommonQObject (Menu s) s QMenu where
    getCommonQObject (Menu coq _) = coq

instance Eq (Menu s) where
    (Menu ptr1 _) == (Menu ptr2 _) = ptr1 == ptr2

class MenuParent a s where
    toQObject :: a -> CommonQObject s QObject

instance MenuParent (Menu s) s where
    toQObject (Menu ptr _) = castCommonQObject ptr

instance MenuParent (MenuBar s) s where
    toQObject (MenuBar{..}) = castCommonQObject menuBarPtr

instance Touchable (Menu s) where
    touch (Menu cqo _) = touch cqo

instance Touchable (MenuBar s) where
    touch (MenuBar cqo _) = touch cqo

-- | Creates a menu bar.
createMenuBar :: MainWindow s -> UIAction s (MenuBar s)
createMenuBar mainwindow = liftIO $ mask_ $
    withCommonQObject mainwindow $ \window_ptr -> do
        mbar <- create_menubar
        set_menubar window_ptr mbar
        cobject <- addCommonQObject mbar Nothing "QMenuBar"
        addChild window_ptr mbar (parentKeepsAlive cobject)
        return $ MenuBar { menuBarPtr = cobject
                         , mainWindow = mainwindow }

-- | Creates a menu and puts it in a menu bar.
createMenu :: T.Text -> MenuBar s -> UIAction s (Menu s)
createMenu title mb = liftIO $ do
    asQString title $ \qstring -> withCommonQObject mb $ \parent -> do
        menu <- create_menu qstring
        add_menubar_menu parent menu
        cobject <- addCommonQObject menu Nothing "QMenu"
        addChild parent menu (parentKeepsAlive cobject)
        return $ Menu cobject mb

-- | Adds an action to a menu.
addMenuAction :: T.Text           -- ^ The text on the action.
              -> Menu s           -- ^ Where to add the action.
              -> UIAction s ()    -- ^ Called when the action is selected.
              -> UIAction s ()
addMenuAction title (Menu cobject _) (UIAction (insulateExceptions -> action)) = liftIO $ mask_ $
    asQString title $ \qstring -> do
        withCommonQObject cobject $ \ptr -> do
            wrapped <- wrapIO action  -- cleaned up by Qt
            add_menu_action ptr qstring wrapped

