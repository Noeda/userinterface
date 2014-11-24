{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.UI.Internal.QString
    ( asQString )
    where

import Control.Monad.Catch
import Graphics.UI.Internal.QTypes
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Foreign.C.Types
import Foreign.Ptr

foreign import ccall unsafe create_qstring
    :: Ptr CChar -> CInt -> IO (Ptr QString)
foreign import ccall unsafe free_qstring
    :: Ptr QString -> IO ()

asQString :: Text -> (Ptr QString -> IO a) -> IO a
asQString txt action = mask $ \restore ->
    B.useAsCStringLen (T.encodeUtf8 txt) $ \(ptr, ptrlen) -> do
        qstr <- create_qstring ptr (fromIntegral ptrlen)
        finally (restore $ action qstr)
                (free_qstring qstr)
{-# INLINEABLE asQString #-}

