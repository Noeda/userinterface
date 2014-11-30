{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.UI.Internal.CentralWidgetable
    ( CentralWidgetable() )
    where

-- | Things that can be the central widget to a main window or a subwidget to
-- an MDI area.
class CentralWidgetable a

