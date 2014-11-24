{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}

module Graphics.UI.MainWindow
    ( createMainWindow
    , MainWindow() )
    where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Ptr
import Graphics.UI.Internal
import Graphics.UI.Internal.QObject
import Graphics.UI.Internal.QTypes
import Graphics.UI.Internal.Touchable

foreign import ccall create_main_window :: IO (Ptr QMainWindow)

newtype MainWindow s = MainWindow (CommonQObject QMainWindow)
                       deriving ( Eq, Typeable )

instance Touchable (MainWindow s) where
    touch (MainWindow cqo) = touch cqo

instance HasCommonQObject (MainWindow s) QMainWindow where
    getCommonQObject (MainWindow cobject) = cobject

instance UIElement (MainWindow s) where
    delete = deleteCommonQObject . getCommonQObject

createMainWindow :: UIAction s (MainWindow s)
createMainWindow = liftIO $ mask_ $ do
    w <- create_main_window
    MainWindow <$> addCommonQObject w (Just $ \ref -> return $ touch ref)

