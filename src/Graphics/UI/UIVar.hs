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
data UIVar s a b = UIVar
    { set :: !(a -> UIAction s ())
    , get :: !(UIAction s b) }

-- | Construct an `UIVar` from a setter and getter.
uivar :: (a -> UIAction s ())
      -> UIAction s b
      -> UIVar s a b
uivar = UIVar

-- | A simple `UIVar`.
type UIVar' s a = UIVar s a a

instance Functor (UIVar s a) where
    fmap f (UIVar setter getter) = UIVar setter (fmap f getter)

instance Profunctor (UIVar s) where
    dimap ab cd (UIVar setter getter) = UIVar
        (\n -> setter $ ab n)
        (fmap cd getter)

-- | Sets the value of an `UIVar`.
(<--) :: UIVar s a b -> a -> UIAction s ()
(<--) = set

-- | Reads the value of an `UIVar`.
readVar :: UIVar s a b -> UIAction s b
readVar = get

