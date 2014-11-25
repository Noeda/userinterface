{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Text areas.
--
-- User can write and edit text areas. These text areas are rich; you can add
-- HTML and fancy stuff in them.
--

module Graphics.UI.TextEdit
    ( createTextEdit
    , readOnly
    , htmlContent
    , TextEdit() )
    where

import Graphics.UI.Internal.Common
import Graphics.UI.UIVar

foreign import ccall create_textedit :: FunPtr (IO ()) -> IO (Ptr QTextEdit)
foreign import ccall set_textedit_readable :: Ptr QTextEdit -> CInt -> IO ()
foreign import ccall get_textedit_readable :: Ptr QTextEdit -> IO CInt
foreign import ccall get_textedit_html :: Ptr QTextEdit -> IO (Ptr QString)
foreign import ccall set_textedit_html :: Ptr QTextEdit -> Ptr QString -> IO ()

newtype TextEdit s = TextEdit { textEditPtr :: CommonQObject s QTextEdit }

instance HasCommonQObject (TextEdit s) s QTextEdit where
    getCommonQObject = textEditPtr

instance UIElement (TextEdit s) s where
    delete = deleteCommonQObject . getCommonQObject
    qwidget = castCommonQObject . getCommonQObject

instance CentralWidgetable (TextEdit s) s QTextEdit where
    centralWidgetableProof _ = ()

-- | Creates a text edit. Initially it is editable by the user.
createTextEdit :: UIAction s () -> UIAction s (TextEdit s)
createTextEdit (UIAction action) = liftIO $ mask_ $ do
    wrapped <- wrapAndInsulateIO action
    tedit <- create_textedit wrapped
    cobject <- addCommonQObject tedit Nothing "QTextEdit"
    return $ TextEdit cobject

-- | Gets and sets HTML content in a text edit.
--
-- See <http://qt-project.org/doc/qt-5/richtext-html-subset.html> documentation
-- on the Qt website to see what the limitations are.
htmlContent :: TextEdit s -> UIVar' s Text
htmlContent te = uivar
    (\htmlcontent -> liftIO $
        withCommonQObject te $ \te_ptr ->
            asQString htmlcontent $ \qstring ->
                set_textedit_html te_ptr qstring)
    (mask_ $ liftIO $ do
        result <- withCommonQObject te $ get_textedit_html
        finally (peekQString result) (freeQString result))

-- | Gets and sets read-only property of a text edit.
readOnly :: TextEdit s -> UIVar' s Bool
readOnly te = uivar
    (\readonly -> liftIO $
        withCommonQObject te $ \te_ptr ->
            set_textedit_readable te_ptr $ if readonly then 1 else 0)
    (do result <- liftIO $ withCommonQObject te $ get_textedit_readable
        return $ result /= 0)

