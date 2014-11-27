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
import Graphics.UI.DockWidget
import Graphics.UI.MainWindow
import Graphics.UI.MenuBar
import Graphics.UI.MdiArea
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
    --monitor
    runUI $ \ev -> do
        liftIO $ print ev
        case ev of
            Ready -> do
                w <- createMainWindow
                mdi <- createMdiArea
                setCentralWidget mdi w
                m <- createMenuBar w
                m1 <- createMenu "File" m
                addMenuAction "Exit" m1 $ delete w

                te <- createTextEdit $ return ()
                d <- createDockWidget w te
                title d <-- "AAaa"
                return ()
            _ -> return ()

