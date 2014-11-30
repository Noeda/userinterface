-- | UIVars, settable and gettable properties in the `UIAction` monad.
--

module Graphics.UI.UIVar
    ( UIVar()
    , UIVar'
    , uivar
    , (<--)
    , readVar )
    where

import Graphics.UI.Internal
import Data.Profunctor

-- | `UIVar` is something you can get and set but those actions require
-- `UIAction` monad.
data UIVar a b = UIVar
    { set :: !(a -> UIAction ())
    , get :: !(UIAction b) }

-- | Construct an `UIVar` from a setter and getter.
uivar :: (a -> UIAction ())
      -> UIAction b
      -> UIVar a b
uivar = UIVar

-- | A simple `UIVar`.
type UIVar' a = UIVar a a

instance Functor (UIVar a) where
    fmap f (UIVar setter getter) = UIVar setter (fmap f getter)

instance Profunctor UIVar where
    dimap ab cd (UIVar setter getter) = UIVar
        (\n -> setter $ ab n)
        (fmap cd getter)

-- | Sets the value of an `UIVar`.
(<--) :: UIVar a b -> a -> UIAction ()
(<--) = set

-- | Reads the value of an `UIVar`.
readVar :: UIVar a b -> UIAction b
readVar = get

