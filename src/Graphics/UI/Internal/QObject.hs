{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Graphics.UI.Internal.QObject
    ( ManagedQObject()
    , coerceManagedQObject
    , createRoot
    , createTrackedQObject
    , Deleteable(..)
    , unRoot
    , manageQObject
    , withManagedQObject
    , garbageCollectionCycle
    , HasManagedQObject(..)
    , HasQObject(..) )
    where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Traversable
import Data.Typeable
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.UI.Internal.QTypes
import Graphics.UI.Internal.Touchable
import System.IO.Unsafe

foreign import ccall "wrapper" wrapIO :: IO () -> IO (FunPtr (IO ()))
foreign import ccall link_destroy_event :: Ptr QObject -> (FunPtr (IO ())) -> IO ()
foreign import ccall post_delete_event :: Ptr QObject -> IO ()
foreign import ccall get_qobject_parent :: Ptr QObject -> IO (Ptr QObject)
foreign import ccall number_of_qobject_children :: Ptr QObject -> IO CInt
foreign import ccall get_qobject_children ::
    Ptr QObject -> (Ptr (Ptr QObject)) -> IO ()

data Visitation = Visitation
    { _toBeVisited :: S.Set (Ptr QObject)
    , _visited :: S.Set (Ptr QObject) }
makeLenses ''Visitation

class HasQObject a where
    getQObject :: a -> TVar (Ptr QObject)

instance HasQObject (TVar (Ptr QObject)) where
    getQObject = id

class Deleteable a where
    delete :: MonadIO m => a -> m ()

instance HasQObject a => Deleteable a where
    delete = liftIO . deleteQObject . getQObject

data ManagedQObject a = ManagedQObject
    { _qobject :: TVar (Ptr QObject)
    , _finalizerRef :: IORef () }
    deriving ( Eq, Functor, Typeable )

coerceManagedQObject :: ManagedQObject a -> ManagedQObject b
coerceManagedQObject (ManagedQObject{..}) = ManagedQObject
    { _qobject = _qobject
    , _finalizerRef = _finalizerRef }

class HasManagedQObject a b | a -> b where
    getManagedQObject :: a -> ManagedQObject b

instance HasManagedQObject (ManagedQObject a) a where
    getManagedQObject = id

instance HasQObject (ManagedQObject a) where
    getQObject = _qobject

instance Touchable (ManagedQObject a) where
    touch x = touch (_finalizerRef x)

manageQObject :: TVar (Ptr QObject) -> IO (ManagedQObject a)
manageQObject tvar = do
    ref <- newIORef ()
    void $ mkWeakIORef ref $ atomically $ do
        ptr <- readTVar tvar
        modifyTVar referencedRoots $ S.delete ptr

    return ManagedQObject {
             _qobject = tvar
           , _finalizerRef = ref
           }

withManagedQObject :: HasManagedQObject a c => a -> (Ptr c -> IO b) -> IO b
withManagedQObject (getManagedQObject -> x) action =
    flip finally (touch x) $ do
        qop <- atomically $ readTVar (getQObject x)
        if qop == nullPtr
          then error "withManagedQObject: QObject* is NULL."
          else action (castPtr qop)

-- Tracks QObjects. The inner TVars are those that are in CommonQObjects
trackedQObjects :: TVar (M.Map (Ptr QObject) (TVar (Ptr QObject)))
trackedQObjects = unsafePerformIO $ newTVarIO M.empty
{-# NOINLINE trackedQObjects #-}

-- invariant (pseudo-code):
--
-- S.member x garbageCollecionRoots --> M.member x trackedQObjects
--
-- That is, anything referenced in garbageCollectionRoots should also be in
-- trackedQObjects. The reverse does not need to be true and isn't.
--

garbageCollectionRoots :: TVar (S.Set (Ptr QObject))
garbageCollectionRoots = unsafePerformIO $ newTVarIO S.empty
{-# NOINLINE garbageCollectionRoots #-}

referencedRoots :: TVar (S.Set (Ptr QObject))
referencedRoots = unsafePerformIO $ newTVarIO S.empty
{-# NOINLINE referencedRoots #-}

emptyVisitation :: Visitation
emptyVisitation = Visitation S.empty S.empty

-- | Adds a root. The pointer should equal the pointer inside the TVar.
--
-- Also adds it as a tracked QObject so no need to call `addTrackedQObject`.
addRoot :: Ptr QObject -> TVar (Ptr QObject) -> STM ()
addRoot pointer tvar_root = do
    modifyTVar trackedQObjects (M.insert pointer tvar_root)
    modifyTVar garbageCollectionRoots (S.insert pointer)
    modifyTVar referencedRoots (S.insert pointer)

-- | Adds a tracked QObject. It will be automatically deleted when deleted from
-- Qt's side. (The TVar will be set to null when this happens).
addTrackedQObject :: Ptr QObject -> TVar (Ptr QObject) -> STM ()
addTrackedQObject pointer tvar_root = do
    modifyTVar trackedQObjects (M.insert pointer tvar_root)
    modifyTVar referencedRoots (S.insert pointer)

unRoot :: TVar (Ptr QObject) -> IO ()
unRoot tvar = atomically $ do
    p <- readTVar tvar
    unless (p == nullPtr) $ do
        modifyTVar garbageCollectionRoots $ S.delete p

-- | A helper function to turn a Ptr QObject into a root and a tvar.
createRoot :: Ptr a -> IO (TVar (Ptr QObject))
createRoot (castPtr -> pointer) = mask_ $ do
    tvar <- atomically $ do
        tvar <- newTVar pointer
        addRoot pointer tvar
        return tvar
    trigger <- wrapIO $ objectGotDestroyed tvar
    link_destroy_event pointer trigger
    return tvar

-- | A helper function to turn a Ptr QObject into a tracked QObject.
createTrackedQObject :: Ptr a -> IO (TVar (Ptr QObject))
createTrackedQObject (castPtr -> pointer) = mask_ $ do
    tvar <- atomically $ do
        tvar <- newTVar pointer
        addTrackedQObject pointer tvar
        return tvar
    trigger <- wrapIO $ objectGotDestroyed tvar
    link_destroy_event pointer trigger
    return tvar

deleteQObject :: TVar (Ptr QObject) -> IO ()
deleteQObject tvar = (=<<) maybeDeleteIt $ atomically $ do
    ptr <- readTVar tvar
    unless (ptr == nullPtr) $ do
        modifyTVar garbageCollectionRoots $ S.delete ptr
        modifyTVar trackedQObjects $ M.delete ptr
        modifyTVar referencedRoots $ S.delete ptr
    return $ if ptr == nullPtr
      then Nothing
      else Just ptr
  where
    maybeDeleteIt Nothing = return ()
    maybeDeleteIt (Just ptr) = post_delete_event ptr

whenDebugging :: IO () -> IO ()
whenDebugging action = action

showGCStats :: String -> IO ()
showGCStats prelude = do
    putStrLn prelude
    roots <- atomically $ readTVar garbageCollectionRoots
    putStrLn $ "Roots (" ++ show (S.size roots) ++ ") : " ++ show roots
    refs <- atomically $ readTVar referencedRoots
    putStrLn $ "References (" ++ show (S.size refs) ++ ") : " ++ show refs
    tracked <- fmap (filter (/= nullPtr) . M.elems) $ atomically $ do
        tracked <- readTVar trackedQObjects
        for tracked $ readTVar
    putStrLn $ "Tracked QObjects (" ++ show (length tracked) ++ ") : " ++ show tracked

garbageCollectionCycle :: IO ()
garbageCollectionCycle = mask_ $ do
    whenDebugging $ showGCStats "---garbageCollectionCycle START---"

    -- remove objects that are already dead
    removeDeadTrackedObjects

    roots <- atomically $ S.union <$> readTVar garbageCollectionRoots <*>
                                      readTVar referencedRoots
    -- collect objects that are alive by roots
    alive <- _visited <$>
             execStateT collectAliveObjects emptyVisitation {
                                            _toBeVisited = roots
                                            }

    dead_pointers <- atomically $ do
        tracked_objects <- readTVar trackedQObjects
        -- so these must be dead? or to be killed
        let dead = M.keysSet tracked_objects `S.difference` alive
        -- bring out your dead!
        the_dead <- S.fromList . catMaybes <$>
                    traverse
                    (\ptr -> do
                        case M.lookup ptr tracked_objects of
                            Nothing -> return Nothing
                            Just tvar -> do
                                r <- readTVar tvar
                                writeTVar tvar nullPtr
                                return $ Just r)
                    (S.toList dead)
        writeTVar trackedQObjects $ M.difference tracked_objects
                                                 (M.fromSet (const ()) dead)
        return the_dead

    for_ dead_pointers post_delete_event

    whenDebugging $ showGCStats "---garbageCollectionCycle END---"

collectAliveObjects :: StateT Visitation IO ()
collectAliveObjects = do
    what_to_visit <- use toBeVisited
    unless (S.null what_to_visit) $ do
        let visit_this = S.elemAt 0 what_to_visit
        toBeVisited %= S.delete visit_this
        visited %= S.insert visit_this
        visit visit_this
        collectAliveObjects
  where
    visit thing = do
        num_children <- liftIO $ number_of_qobject_children thing
        when (num_children > 0) $ do
            kids <- liftIO $ allocaArray (fromIntegral num_children) $ \arr -> do
                get_qobject_children thing arr
                peekArray (fromIntegral num_children) arr
            traverse_ scheduleVisitIfNotVisited kids
        parent <- liftIO $ get_qobject_parent thing
        when (parent /= nullPtr) $
            scheduleVisitIfNotVisited parent

    scheduleVisitIfNotVisited visit_me = do
        already_visited <- use visited
        unless (S.member visit_me already_visited) $
            toBeVisited %= S.insert visit_me

removeDeadTrackedObjects :: IO ()
removeDeadTrackedObjects = atomically $ do
    tracked_objects <- readTVar trackedQObjects
    new_tracked_objects <- rec (M.assocs tracked_objects) tracked_objects
    writeTVar trackedQObjects new_tracked_objects
    modifyTVar garbageCollectionRoots
               (S.intersection $ M.keysSet new_tracked_objects)
  where
    rec [] !tracked_objects = return tracked_objects
    rec ((key, tvar):rest) !tracked_objects = do
        actual_ptr <- readTVar tvar
        rec rest $ if actual_ptr == nullPtr
          then M.delete key tracked_objects
          else tracked_objects

-- exports for Qt side
--

-- called when destroyed() signal is triggered in Qt
objectGotDestroyed :: TVar (Ptr QObject) -> IO ()
objectGotDestroyed thing = atomically $ do
    ptr <- readTVar thing
    unless (ptr == nullPtr) $ do
        writeTVar thing nullPtr
        modifyTVar trackedQObjects (M.delete ptr)
        modifyTVar garbageCollectionRoots $ S.delete ptr
        modifyTVar referencedRoots $ S.delete ptr

