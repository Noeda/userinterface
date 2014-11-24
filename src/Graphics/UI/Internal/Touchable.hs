{-# LANGUAGE BangPatterns #-}

module Graphics.UI.Internal.Touchable
    ( Touchable(..) )
    where

import Data.IORef

class Touchable a where
    touch :: a -> IO ()

instance Touchable (IORef a) where
    touch !ref = return ()
    {-# NOINLINE touch #-}

