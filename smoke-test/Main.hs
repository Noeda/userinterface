{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Exception ( getMaskingState )
import Control.Monad.IO.Class
import Data.Foldable
import Data.Monoid
import qualified Data.Text as T
import Graphics.UI
import Graphics.UI.Internal.QObject
import Graphics.UI.DockWidget
import Graphics.UI.MainWindow
import Graphics.UI.MdiArea
import Graphics.UI.MenuBar
import Graphics.UI.TextEdit
import Graphics.UI.Timer
import Graphics.UI.UIVar
import Graphics.UI.Widget
import System.Mem
import Data.IORef
import Data.Dynamic
import System.IO

data Blah = Blah deriving ( Eq, Ord, Show, Read, Typeable )
instance Exception Blah

main :: IO ()
main = do
    monitor
    runUI $ \ev -> do
        liftIO $ print ev
        case ev of
            Ready -> do
                w <- createMainWindow
                mdi <- createMdiArea
                te <- createTextEdit $ return ()
                htmlContent te <-- "Heiheih"
                setCentralWidget mdi w
                dwidget <- createDockWidget w te
                title dwidget <-- "Docking"
                replicateM_ 5 $ do
                    te <- createTextEdit $ liftIO $ print =<< getMaskingState
                    htmlContent te <-- "<a href=\"123\">hei</a>"
                    addSubWidget te mdi
                mb <- createMenuBar w
                m <- createMenu "File" mb
                for_ [1..10] $ \i -> addMenuAction (T.pack $ show i)
                                                   m
                                                   (liftIO $ print i)
                addMenuAction "Exit" m (deleteUIElement w)
            _ -> return ()

