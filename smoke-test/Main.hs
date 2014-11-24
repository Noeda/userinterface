{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Monoid
import qualified Data.Text as T
import Graphics.UI
import Graphics.UI.Internal.QObject
import Graphics.UI.MainWindow
import Graphics.UI.MenuBar
import Graphics.UI.Timer
import System.Mem
import Data.IORef
import Data.Dynamic

main :: IO ()
main = do
    runUI $ \ev -> do
        liftIO $ print ev
        case ev of
            Ready -> do
                w <- createMainWindow
                mb <- createMenuBar w
                m <- createMenu "File" mb
                timerLoop mb
                for_ [1..10] $ \i -> addMenuAction (T.pack $ show i)
                                                m
                                                (liftIO $ print i)
                addMenuAction "Exit" m (deleteUIElement w)
            _ -> return ()

timerLoop :: MenuBar s -> UIAction s ()
timerLoop mb = do
    ref <- liftIO $ newIORef 0
    void $ createTimer False 1000 $ do
        x <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (x+1)
        m <- createMenu ("Toot" <> (T.pack $ show x)) mb
        for_ [1..10] $ \i -> addMenuAction (T.pack $ show (x+i))
                                           m
                                           (liftIO $ print (x*i))

