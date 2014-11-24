{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Graphics.UI.MenuBar
    ( createMenuBar
    , createMenu
    , addMenuAction
    , MenuBar()
    , Menu()
    , MenuParent() )
    where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Dynamic
import qualified Data.Text as T
import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Ptr
import Graphics.UI.Internal
import Graphics.UI.Internal.QObject
import Graphics.UI.Internal.QString
import Graphics.UI.Internal.QTypes
import Graphics.UI.Internal.Touchable
import Graphics.UI.MainWindow

foreign import ccall create_menubar :: IO (Ptr QMenuBar)
foreign import ccall create_menu :: Ptr QString -> IO (Ptr QMenu)
foreign import ccall add_menubar_menu :: Ptr QMenuBar -> Ptr QMenu -> IO ()
foreign import ccall set_menubar :: Ptr QMainWindow -> Ptr QMenuBar -> IO ()
foreign import ccall add_menu_action ::
    Ptr QMenu -> Ptr QString -> FunPtr (IO ()) -> IO ()
foreign export ccall freeHaskellFunPtr :: FunPtr a -> IO ()
foreign import ccall "wrapper" wrapIO :: IO () -> IO (FunPtr (IO ()))

data MenuBar s = MenuBar { menuBarPtr :: CommonQObject QMenuBar
                         , mainWindow :: MainWindow s }
                 deriving ( Eq, Typeable )

instance HasCommonQObject (MenuBar s) QMenuBar where
    getCommonQObject = menuBarPtr

instance UIElement (MenuBar s) where
    delete = deleteCommonQObject . getCommonQObject

instance UIElement (Menu s) where
    delete = deleteCommonQObject . getCommonQObject

data Menu s where
    Menu :: MenuParent a => CommonQObject QMenu -> a -> Menu s

instance HasCommonQObject (Menu s) QMenu where
    getCommonQObject (Menu coq _) = coq

instance Eq (Menu s) where
    (Menu ptr1 _) == (Menu ptr2 _) = ptr1 == ptr2

class MenuParent a where
    toQObject :: a -> CommonQObject QObject

instance MenuParent (Menu s) where
    toQObject (Menu ptr _) = castCommonQObject ptr

instance MenuParent (MenuBar s) where
    toQObject (MenuBar{..}) = castCommonQObject menuBarPtr

instance Touchable (Menu s) where
    touch (Menu cqo _) = touch cqo

instance Touchable (MenuBar s) where
    touch (MenuBar cqo _) = touch cqo

createMenuBar :: MainWindow s -> UIAction s (MenuBar s)
createMenuBar mainwindow = liftIO $ mask_ $
    withCommonQObject mainwindow $ \window_ptr -> do
        mbar <- create_menubar
        set_menubar window_ptr mbar
        cobject <- addCommonQObject mbar Nothing
        addChild window_ptr mbar (touch cobject)
        return $ MenuBar { menuBarPtr = cobject
                         , mainWindow = mainwindow }

createMenu :: T.Text -> MenuBar s -> UIAction s (Menu s)
createMenu title mb = liftIO $ do
    asQString title $ \qstring -> withCommonQObject mb $ \parent -> do
        menu <- create_menu qstring
        add_menubar_menu parent menu
        cobject <- addCommonQObject menu Nothing
        addChild parent menu (touch cobject)
        return $ Menu cobject mb

addMenuAction :: T.Text -> Menu s -> UIAction s () -> UIAction s ()
addMenuAction title (Menu cobject mb) (UIAction (insulateExceptions -> action)) = liftIO $ mask_ $
    asQString title $ \qstring -> do
        withCommonQObject cobject $ \ptr -> do
            wrapped <- wrapIO action  -- cleaned up by Qt
            add_menu_action ptr qstring wrapped

