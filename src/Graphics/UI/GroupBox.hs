{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE ViewPatterns #-}

module Graphics.UI.GroupBox
    ( createGroupBox
    , groupBoxAlignment
    , flat
    , checked
    , checkable
    , GroupBox() )
    where

import Graphics.UI.Internal.Common
import Graphics.UI.UIVar
import Graphics.UI.Widget

foreign import ccall create_groupbox :: FunPtr (CInt -> IO ())
                                     -> IO (Ptr QGroupBox)
foreign import ccall set_groupbox_title :: Ptr QGroupBox -> Ptr QString -> IO ()
foreign import ccall get_groupbox_title :: Ptr QGroupBox -> IO (Ptr QString)
foreign import ccall set_groupbox_flat :: Ptr QGroupBox -> CInt -> IO ()
foreign import ccall get_groupbox_flat :: Ptr QGroupBox -> IO CInt
foreign import ccall set_groupbox_checked :: Ptr QGroupBox -> CInt -> IO ()
foreign import ccall get_groupbox_checked :: Ptr QGroupBox -> IO CInt
foreign import ccall set_groupbox_checkable :: Ptr QGroupBox -> CInt -> IO ()
foreign import ccall get_groupbox_checkable :: Ptr QGroupBox -> IO CInt
foreign import ccall set_groupbox_alignment :: Ptr QGroupBox -> CInt -> IO ()
foreign import ccall get_groupbox_alignment :: Ptr QGroupBox -> IO CInt

foreign import ccall "wrapper" wrapToggled
    :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))

newtype GroupBox = GroupBox (ManagedQObject QGroupBox)
                   deriving ( Eq, Typeable, Touchable, HasQObject )

instance CentralWidgetable GroupBox

instance Titleable GroupBox where
    setTitle _ p = set_groupbox_title (castPtr p)
    getTitle _ p = get_groupbox_title (castPtr p)

instance IsWidget GroupBox where
    getWidget = coerceManagedQObject . getManagedQObject

instance HasManagedQObject GroupBox QGroupBox where
    getManagedQObject (GroupBox man) = man

createGroupBox :: (Bool -> UIAction ()) -> UIAction GroupBox
createGroupBox event = liftIO $ mask_ $ do
    wrapped <- wrapToggled $ \b -> let UIAction action =
                                           event (if b /= 0
                                                   then True
                                                   else False)
                                    in insulateExceptions action
    GroupBox <$> (manageQObject =<<
                  createTrackedQObject =<<
                  create_groupbox wrapped)

data GroupBoxAlignment
    = GBAlignLeft
    | GBAlignRight
    | GBAlignHCenter
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

toConstantG :: GroupBoxAlignment -> CInt
toConstantG GBAlignLeft = 0x1
toConstantG GBAlignRight = 0x2
toConstantG GBAlignHCenter = 0x4

fromConstantG :: CInt -> GroupBoxAlignment
fromConstantG 0x1 = GBAlignLeft
fromConstantG 0x2 = GBAlignRight
fromConstantG 0x4 = GBAlignHCenter
fromConstantG _ = error "fromConstantG: invalid value."

groupBoxAlignment :: GroupBox -> UIVar' GroupBoxAlignment
groupBoxAlignment gb = uivar set_it get_it
  where
    set_it (toConstantG -> c) = liftIO $
        withManagedQObject gb $ \gb_ptr ->
            set_groupbox_alignment gb_ptr c

    get_it = liftIO $ withManagedQObject gb $ \gb_ptr ->
        fromConstantG <$> get_groupbox_alignment gb_ptr

flat :: GroupBox -> UIVar' Bool
flat gb = uivar set_it get_it
  where
    set_it s = liftIO $
        withManagedQObject gb $ \gb_ptr ->
            set_groupbox_flat gb_ptr (if s then 1 else 0)

    get_it = liftIO $
        withManagedQObject gb $ \gb_ptr -> do
            r <- get_groupbox_flat gb_ptr
            return $ if r /= 0 then True else False

checked :: GroupBox -> UIVar' Bool
checked gb = uivar set_it get_it
  where
    set_it s = liftIO $
        withManagedQObject gb $ \gb_ptr ->
            set_groupbox_checked gb_ptr (if s then 1 else 0)

    get_it = liftIO $
        withManagedQObject gb $ \gb_ptr -> do
            r <- get_groupbox_checked gb_ptr
            return $ if r /= 0 then True else False

checkable :: GroupBox -> UIVar' Bool
checkable gb = uivar set_it get_it
  where
    set_it s = liftIO $
        withManagedQObject gb $ \gb_ptr ->
            set_groupbox_checkable gb_ptr (if s then 1 else 0)

    get_it = liftIO $
        withManagedQObject gb $ \gb_ptr -> do
            r <- get_groupbox_checkable gb_ptr
            return $ if r /= 0 then True else False

