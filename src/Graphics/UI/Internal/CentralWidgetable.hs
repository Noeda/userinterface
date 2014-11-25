{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.UI.Internal.CentralWidgetable
    ( CentralWidgetable(..) )
    where

import Data.Proxy
import Graphics.UI.Internal.QObject

-- | Things that can be the central widget to a main window or a subwidget to
-- an MDI area.
class HasCommonQObject a s b => CentralWidgetable a s b where
    centralWidgetableProof :: Proxy a -> ()

