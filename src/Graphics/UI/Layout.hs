{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Graphics.UI.Layout
    (
    -- * Box layout
      createBoxLayout
    , addSpacing
    , addStretch
    , addStrut
    , addWidgetToBox
    , direction
    , BoxLayout()
    -- * Form layout
    , createFormLayout
    , addRow
    , FormLayout()
    -- * Stacked layout
    , createStackedLayout
    , addPage
    , page
    , PageIndex()
    , StackedLayout()
    -- * Grid layout
    , createGridLayout
    , addWidgetToGrid
    , originCorner
    , Row
    , Column
    , Corner(..)
    , GridLayout()
    -- * Common layout operations
    -- ** Sizing
    , sizeConstraint
    , SizeConstraint(..)
    -- ** Spacing
    , spacing
    -- ** Margins
    , margins
    , Margins(..)
    -- ** Aligning
    , setAlignment
    , centered
    , Alignment(..)
    , AlignH(..)
    , AlignV(..) )
    where

import Data.Bits
import Foreign.Marshal.Alloc
import Foreign.Storable hiding ( alignment )
import Graphics.UI.Internal.Common
import Graphics.UI.Label
import Graphics.UI.UIVar
import Graphics.UI.Widget

foreign import ccall set_layout_alignment :: Ptr QLayout -> CInt -> IO ()
foreign import ccall set_layout_margins
    :: Ptr QLayout -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign import ccall get_layout_margins
    :: Ptr QLayout -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall set_layout_spacing :: Ptr QLayout -> CInt -> IO ()
foreign import ccall get_layout_spacing :: Ptr QLayout -> IO CInt
foreign import ccall set_layout_size_constraint :: Ptr QLayout -> CInt -> IO ()
foreign import ccall get_layout_size_constraint :: Ptr QLayout -> IO CInt


-- box
foreign import ccall create_box_layout :: IO (Ptr QBoxLayout)
foreign import ccall add_widget_to_box_layout
    :: Ptr QBoxLayout -> Ptr QWidget -> CInt -> CInt -> IO ()
foreign import ccall set_box_layout_direction :: Ptr QBoxLayout -> CInt -> IO ()
foreign import ccall get_box_layout_direction :: Ptr QBoxLayout -> IO CInt
foreign import ccall add_box_strut :: Ptr QBoxLayout -> CInt -> IO ()
foreign import ccall add_box_spacing :: Ptr QBoxLayout -> CInt -> IO ()
foreign import ccall add_box_stretch :: Ptr QBoxLayout -> CInt -> IO ()

-- form
foreign import ccall create_form_layout :: IO (Ptr QFormLayout)
foreign import ccall add_form_row :: Ptr QFormLayout
                                  -> Ptr QLabel
                                  -> Ptr QWidget
                                  -> IO ()

-- stacked
foreign import ccall create_stacked_layout :: IO (Ptr QStackedLayout)
foreign import ccall add_stacked_page :: Ptr QStackedLayout
                                      -> Ptr QWidget
                                      -> IO CInt
foreign import ccall set_stacked_page :: Ptr QStackedLayout -> CInt -> IO ()
foreign import ccall get_stacked_page :: Ptr QStackedLayout -> IO CInt

-- grid
foreign import ccall create_grid_layout :: IO (Ptr QGridLayout)
foreign import ccall set_grid_corner :: Ptr QGridLayout -> CInt -> IO ()
foreign import ccall get_grid_corner :: Ptr QGridLayout -> IO CInt
foreign import ccall add_widget_to_grid_layout
    :: Ptr QGridLayout -> Ptr QWidget -> CInt -> CInt -> IO ()

newtype BoxLayout = BoxLayout (ManagedQObject QBoxLayout)
                      deriving ( Eq, Typeable, Touchable, HasQObject )

newtype FormLayout = FormLayout (ManagedQObject QFormLayout)
                       deriving ( Eq, Typeable, Touchable, HasQObject )

newtype StackedLayout = StackedLayout (ManagedQObject QStackedLayout)
                          deriving ( Eq, Typeable, Touchable, HasQObject )

newtype GridLayout = GridLayout (ManagedQObject QGridLayout)
                       deriving ( Eq, Typeable, Touchable, HasQObject )

instance HasManagedQObject GridLayout QGridLayout where
    getManagedQObject (GridLayout man) = man

instance IsLayout GridLayout where
    getLayout (GridLayout man) = coerceManagedQObject man

instance HasManagedQObject StackedLayout QStackedLayout where
    getManagedQObject (StackedLayout man) = man

instance IsLayout StackedLayout where
    getLayout (StackedLayout man) = coerceManagedQObject man

instance HasManagedQObject FormLayout QFormLayout where
    getManagedQObject (FormLayout man) = man

instance IsLayout FormLayout where
    getLayout (FormLayout man) = coerceManagedQObject man

instance HasManagedQObject BoxLayout QBoxLayout where
    getManagedQObject (BoxLayout man) = man

instance IsLayout BoxLayout where
    getLayout (BoxLayout man) = coerceManagedQObject man

data Alignment = Alignment { horizontal :: !AlignH, vertical :: !AlignV }
                 deriving ( Eq, Ord, Show, Read, Typeable )

centered :: Alignment
centered = Alignment { horizontal = AlignHCenter, vertical = AlignVCenter }

data AlignH
    = AlignLeft
    | AlignRight
    | AlignHCenter
    | AlignJustify
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data AlignV
    = AlignTop
    | AlignBottom
    | AlignVCenter
    | AlignBaseline
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data Margins = Margins
    { left :: !Int
    , top :: !Int
    , right :: !Int
    , bottom :: !Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

toConstant :: Alignment -> CInt
toConstant (Alignment h v) = toConstantH h .|. toConstantV v

-- check that these agree with Qt
-- <http://qt-project.org/doc/qt-5/qt.html#AlignmentFlag-enum>
toConstantH :: AlignH -> CInt
toConstantH AlignLeft = 0x1
toConstantH AlignRight = 0x2
toConstantH AlignHCenter = 0x4
toConstantH AlignJustify = 0x8

toConstantV :: AlignV -> CInt
toConstantV AlignTop = 0x20
toConstantV AlignBottom = 0x40
toConstantV AlignVCenter = 0x80
toConstantV AlignBaseline = 0x100

{-fromConstant :: CInt -> Alignment
fromConstant v =
    Alignment { horizontal = fromConstantH (v .&. 0xf)
              , vertical = fromConstantV (v .&. 0xff0) }-}

{-fromConstantH :: CInt -> AlignH
fromConstantH 0x1 = AlignLeft
fromConstantH 0x2 = AlignRight
fromConstantH 0x4 = AlignHCenter
fromConstantH 0x8 = AlignJustify
fromConstantH _ = error "fromConstantH: invalid value."

fromConstantV :: CInt -> AlignV
fromConstantV 0x20 = AlignTop
fromConstantV 0x40 = AlignBottom
fromConstantV 0x80 = AlignVCenter
fromConstantV 0x100 = AlignBaseline
fromConstantV _ = error "fromConstantV: invalid value."-}

data Direction
    = LeftToRight
    | RightToLeft
    | TopToBottom
    | BottomToTop
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

toConstantD :: Direction -> CInt
toConstantD LeftToRight = 0
toConstantD RightToLeft = 1
toConstantD TopToBottom = 2
toConstantD BottomToTop = 3

fromConstantD :: CInt -> Direction
fromConstantD 0 = LeftToRight
fromConstantD 1 = RightToLeft
fromConstantD 2 = TopToBottom
fromConstantD 3 = BottomToTop
fromConstantD _ = error "fromConstantD: invalid value."

direction :: BoxLayout -> UIVar' Direction
direction boxl = uivar set_it get_it
  where
    set_it (toConstantD -> dir) =
        liftIO $ withManagedQObject boxl $ \boxl_ptr ->
            set_box_layout_direction boxl_ptr dir
    get_it = liftIO $ withManagedQObject boxl $ \boxl_ptr ->
        fromConstantD <$> get_box_layout_direction boxl_ptr

spacing :: IsLayout a => a -> UIVar' Int
spacing (getLayout -> layout) = uivar set_it get_it
  where
    set_it new_spacing =
        liftIO $ withManagedQObject layout $ \layout_ptr ->
            set_layout_spacing layout_ptr $ fromIntegral new_spacing
    get_it =
        liftIO $ withManagedQObject layout $ \layout_ptr ->
            fromIntegral <$> get_layout_spacing layout_ptr

setAlignment :: IsLayout a => Alignment -> a -> UIAction ()
setAlignment (toConstant -> c) (getLayout -> layout) =
    liftIO $ withManagedQObject layout $ \layout_ptr ->
        set_layout_alignment layout_ptr c

margins :: IsLayout a => a -> UIVar' Margins
margins (getLayout -> layout) = uivar set_it get_it
  where
    set_it (Margins{..}) =
        liftIO $ withManagedQObject layout $ \layout_ptr ->
            set_layout_margins layout_ptr
                               (fromIntegral left)
                               (fromIntegral top)
                               (fromIntegral right)
                               (fromIntegral bottom)
    get_it =
        liftIO $ withManagedQObject layout $ \layout_ptr ->
            alloca $ \left_ptr ->
            alloca $ \top_ptr ->
            alloca $ \right_ptr ->
            alloca $ \bottom_ptr -> do
                get_layout_margins layout_ptr
                                   left_ptr
                                   top_ptr
                                   right_ptr
                                   bottom_ptr
                Margins <$> peekI left_ptr <*>
                            peekI top_ptr <*>
                            peekI right_ptr <*>
                            peekI bottom_ptr

    peekI x = fromIntegral <$> peek x

addWidgetToBox :: IsWidget a
               => a
               -> Int
               -> Int
               -> BoxLayout
               -> UIAction ()
addWidgetToBox (getWidget -> widget) align stretching boxl = liftIO $
    withManagedQObject boxl $ \boxl_ptr ->
    withManagedQObject widget $ \widget_ptr ->
        add_widget_to_box_layout boxl_ptr widget_ptr (fromIntegral align)
                                                     (fromIntegral stretching)

addSpacing :: Int -> BoxLayout -> UIAction ()
addSpacing how_much boxl = liftIO $ withManagedQObject boxl $ \boxl_ptr ->
    add_box_spacing boxl_ptr (fromIntegral how_much)

addStretch :: Int -> BoxLayout -> UIAction ()
addStretch factor boxl = liftIO $ withManagedQObject boxl $ \boxl_ptr ->
    add_box_stretch boxl_ptr (fromIntegral factor)

addStrut :: Int -> BoxLayout -> UIAction ()
addStrut strut boxl = liftIO $ withManagedQObject boxl $ \boxl_ptr ->
    add_box_strut boxl_ptr (fromIntegral strut)

createBoxLayout :: UIAction BoxLayout
createBoxLayout = liftIO $
    BoxLayout <$> (manageQObject =<< createTrackedQObject =<< create_box_layout)

createFormLayout :: UIAction FormLayout
createFormLayout = liftIO $
    FormLayout <$> (manageQObject =<< createTrackedQObject =<< create_form_layout)

createStackedLayout :: UIAction StackedLayout
createStackedLayout = liftIO $
    StackedLayout <$> (manageQObject =<< createTrackedQObject =<< create_stacked_layout)

createGridLayout :: UIAction GridLayout
createGridLayout = liftIO $
    GridLayout <$> (manageQObject =<< createTrackedQObject =<< create_grid_layout)

addRow :: IsWidget a => Label -> a -> FormLayout -> UIAction ()
addRow label (getWidget -> widget) forml = liftIO $
    withManagedQObject forml $ \forml_ptr ->
    withManagedQObject label $ \label_ptr ->
    withManagedQObject widget $ \widget_ptr ->
        add_form_row forml_ptr label_ptr widget_ptr

newtype PageIndex = PageIndex CInt
                    deriving ( Eq, Ord, Show, Typeable )

addPage :: IsWidget a => a -> StackedLayout -> UIAction PageIndex
addPage (getWidget -> widget) stackedl = liftIO $
    withManagedQObject stackedl $ \stackedl_ptr ->
    withManagedQObject widget $ \widget_ptr ->
        PageIndex <$> add_stacked_page stackedl_ptr widget_ptr

page :: StackedLayout -> UIVar' PageIndex
page stackedl = uivar set_it get_it
  where
    set_it (PageIndex page_index) = liftIO $
        withManagedQObject stackedl $ \stackedl_ptr ->
            set_stacked_page stackedl_ptr page_index
    get_it = liftIO $
        withManagedQObject stackedl $ \stackedl_ptr ->
            PageIndex <$> get_stacked_page stackedl_ptr

data Corner
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

toConstantCo :: Corner -> CInt
toConstantCo TopLeft = 0x0
toConstantCo TopRight = 0x1
toConstantCo BottomLeft = 0x2
toConstantCo BottomRight = 0x3

fromConstantCo :: CInt -> Corner
fromConstantCo 0x0 = TopLeft
fromConstantCo 0x1 = TopRight
fromConstantCo 0x2 = BottomLeft
fromConstantCo 0x3 = BottomRight
fromConstantCo _ = error "fromConstantCo: invalid value."

originCorner :: GridLayout -> UIVar' Corner
originCorner gridl = uivar set_it get_it
  where
    set_it (toConstantCo -> corner) = liftIO $
        withManagedQObject gridl $ \gridl_ptr ->
            set_grid_corner gridl_ptr corner
    get_it = liftIO $
        withManagedQObject gridl $ \gridl_ptr ->
            fromConstantCo <$> get_grid_corner gridl_ptr

type Row = Int
type Column = Int

addWidgetToGrid :: IsWidget a
                => a
                -> Row
                -> Column
                -> GridLayout
                -> UIAction ()
addWidgetToGrid (getWidget -> widget) row column gridl = liftIO $
    withManagedQObject widget $ \widget_ptr ->
    withManagedQObject gridl $ \gridl_ptr ->
        add_widget_to_grid_layout gridl_ptr widget_ptr
                                  (fromIntegral row) (fromIntegral column)

data SizeConstraint
    = Default
    | FixedSize
    | MinimumSize
    | MaximumSize
    | MinimumAndMaximumSize
    | NoConstraint
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

toConstantSC :: SizeConstraint -> CInt
toConstantSC Default = 0
toConstantSC FixedSize = 3
toConstantSC MinimumSize = 2
toConstantSC MaximumSize = 4
toConstantSC MinimumAndMaximumSize = 5
toConstantSC NoConstraint = 1

fromConstantSC :: CInt -> SizeConstraint
fromConstantSC 0 = Default
fromConstantSC 3 = FixedSize
fromConstantSC 2 = MinimumSize
fromConstantSC 4 = MaximumSize
fromConstantSC 5 = MinimumAndMaximumSize
fromConstantSC 1 = NoConstraint
fromConstantSC _ = error "fromConstantSC: invalid value."

sizeConstraint :: IsLayout a => a -> UIVar' SizeConstraint
sizeConstraint (getLayout -> layout) = uivar set_it get_it
  where
    set_it (toConstantSC -> sc) = liftIO $
        withManagedQObject layout $ \layout_ptr ->
            set_layout_size_constraint layout_ptr sc

    get_it = liftIO $
        withManagedQObject layout $ \layout_ptr ->
            fromConstantSC <$> get_layout_size_constraint layout_ptr

