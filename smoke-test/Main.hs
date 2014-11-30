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
import Graphics.UI.GroupBox
import Graphics.UI.Label
import Graphics.UI.Layout
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
                m1 <- createMenu "&File" m
                addMenuAction "Exit" m1 $ delete w

                box <- createGroupBox $ \x -> liftIO $ print x
                title box <-- "Boksy"
                checkable box <-- True

                sw <- addSubWidget box mdi
                Just sw_layout <- layout sw
                liftIO . print =<< readVar (sizeConstraint sw_layout)
                sizeConstraint sw_layout <-- FixedSize
                title sw <-- "Buksy"

                b <- createBoxLayout

                te2 <- createTextEdit $ return ()
                addSubWidget te2 mdi

                lab <- createLabel
                text lab <-- "Hello"
                d <- createDockWidget w lab
                title d <-- "AA&aa"
                return ()
            _ -> return ()

