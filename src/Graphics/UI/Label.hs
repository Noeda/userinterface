{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Graphics.UI.Label
    ( createLabel
    , text
    , interpretation
    , setBuddy
    , Label() )
    where

import Data.Text
import Graphics.UI.Internal.Common
import Graphics.UI.UIVar
import Graphics.UI.Widget

foreign import ccall create_label :: IO (Ptr QLabel)
foreign import ccall set_label_buddy :: Ptr QLabel -> Ptr QWidget -> IO ()
foreign import ccall set_label_text :: Ptr QLabel -> Ptr QString -> IO ()
foreign import ccall get_label_text :: Ptr QLabel -> IO (Ptr QString)
foreign import ccall set_label_interpretation
    :: Ptr QLabel -> CInt -> IO ()
foreign import ccall get_label_interpretation
    :: Ptr QLabel -> IO CInt

newtype Label = Label (ManagedQObject QLabel)
                deriving ( Eq, Typeable, HasQObject, Touchable )

instance HasManagedQObject Label QLabel where
    getManagedQObject (Label man) = man

instance CentralWidgetable Label

instance IsWidget Label where
    getWidget = coerceManagedQObject . getManagedQObject

-- | Creates a label. The label initially has no text.
createLabel :: UIAction Label
createLabel = liftIO $ mask_ $ do
    label_ptr <- create_label
    Label <$> (manageQObject =<< createTrackedQObject label_ptr)

-- | Set the buddy of the label.
--
-- See <http://qt-project.org/doc/qt-5/qlabel.html#setBuddy>.
setBuddy :: HasManagedQObject a b => Label -> a -> UIAction ()
setBuddy label thing = liftIO $
    withManagedQObject label $ \label_ptr ->
    withManagedQObject thing $ \(castPtr -> thing_ptr) ->
        set_label_buddy label_ptr thing_ptr

-- | `UIVar` to label's text.
text :: Label -> UIVar' Text
text label = uivar
    (\new_text -> liftIO $
        withManagedQObject label $ \label_ptr ->
            asQString new_text $ set_label_text label_ptr)
    (liftIO $ mask_ $ withManagedQObject label $ \label_ptr -> do
        str <- get_label_text label_ptr
        ret <- peekQString str
        freeQString str
        return ret)

data LabelInterpretation
    = PlainText
    | RichText
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

-- Make these agree on the C++ side as well
toConstant :: LabelInterpretation -> CInt
toConstant PlainText = 0
toConstant RichText = 1

fromConstant :: CInt -> LabelInterpretation
fromConstant 0 = PlainText
fromConstant 1 = RichText
fromConstant _ = error "fromConstant: invalid label interpretation."

-- | `UIVar` to interpret the label's text
interpretation :: Label -> UIVar' LabelInterpretation
interpretation label = uivar
    (\new_interpretation -> liftIO $
        withManagedQObject label $ \label_ptr ->
            set_label_interpretation label_ptr (toConstant new_interpretation))
    (liftIO $ withManagedQObject label $ \label_ptr ->
        fromConstant <$> get_label_interpretation label_ptr)

