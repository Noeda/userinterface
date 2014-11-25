{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.UI.Internal.QString
    ( asQString
    , peekQString
    , freeQString )
    where

import Control.Exception ( evaluate )
import Control.Monad.Catch
import Graphics.UI.Internal.QTypes
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Text ( Text )
import qualified Data.Text.Encoding as T
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.ByteOrder

foreign import ccall unsafe create_qstring
    :: Ptr CChar -> CInt -> IO (Ptr QString)
foreign import ccall unsafe free_qstring
    :: Ptr QString -> IO ()
foreign import ccall unsafe get_qstring_utf16_size
    :: Ptr QString -> IO CInt
foreign import ccall unsafe get_qstring_as_utf16
    :: Ptr QString -> IO (Ptr CChar)

asQString :: Text -> (Ptr QString -> IO a) -> IO a
asQString txt action = mask $ \restore ->
    B.useAsCStringLen (T.encodeUtf8 txt) $ \(ptr, ptrlen) -> do
        qstr <- create_qstring ptr (fromIntegral ptrlen)
        finally (restore $ action qstr)
                (free_qstring qstr)
{-# INLINEABLE asQString #-}

freeQString :: Ptr QString -> IO ()
freeQString = free_qstring

peekQString :: Ptr QString -> IO Text
peekQString qstring = do
    sz <- get_qstring_utf16_size qstring
    utf16 <- get_qstring_as_utf16 qstring
    bs <- B.unsafePackCStringLen (utf16, fromIntegral sz * sizeOf (undefined :: Word16))
    let x = case byteOrder of
              BigEndian -> T.decodeUtf16BE bs
              LittleEndian -> T.decodeUtf16LE bs
              _ -> error "peekQString: mixed endian CPUs are not supported."
    evaluate x
{-# INLINEABLE peekQString #-}

