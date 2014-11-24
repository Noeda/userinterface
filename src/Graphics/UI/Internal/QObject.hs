{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.UI.Internal.QObject
    ( addQObject
    , deleteQObject
    , deleteCommonQObject
    , defaultFinalizer
    , CommonQObject()
    , withCommonQObject
    , addChild
    , castCommonQObject
    , HasCommonQObject(..)
    , addCommonQObject
    , monitor )
    where

import Control.Concurrent
import Control.Lens hiding ( children )
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as S
import Data.Foldable
import Data.Typeable
import Foreign.Ptr
import Graphics.UI.Internal
import Graphics.UI.Internal.Touchable
import Graphics.UI.Internal.QTypes
import System.IO.Unsafe
import System.Mem
import Unsafe.Coerce

foreign import ccall post_delete_event :: Ptr QObject -> IO ()

data QObjectWorld = QObjectWorld
    { _qobjects :: !(M.Map (Ptr QObject) QObjectInfo) }

data QObjectInfo = QObjectInfo
    { _children :: !(M.Map (Ptr QObject) (IO ()))
    , _parent :: !(Ptr QObject)
    , _holder :: !(IO ())
    , _invalidators :: [IO ()] }
makeLenses ''QObjectWorld
makeLenses ''QObjectInfo

globalQObjectWorld :: MVar QObjectWorld
globalQObjectWorld = unsafePerformIO $ newMVar $ QObjectWorld M.empty
{-# NOINLINE globalQObjectWorld #-}

monitor :: IO ()
monitor = void $ forkIO $ forever $ do
    world <- (M.assocs . _qobjects) `fmap` readMVar globalQObjectWorld
    putStrLn "-start-"
    for_ world $ \(ptr, oinfo) ->
        print (ptr, (M.keys $ _children oinfo), length (_invalidators oinfo))
    putStrLn "-end-"
    threadDelay 100000

addQObject :: Ptr QObject
           -> Ptr QObject
           -> M.Map (Ptr QObject) (IO ())
           -> IO ()
           -> Maybe (IO ())
           -> IO ()
addQObject qobject parent_ptr children finalizer !holder =
    modifyMVar_ globalQObjectWorld $ \world ->
        if M.member qobject (_qobjects world)
          then error "addQObject: QObject already part of the world."
          else let holding = case holder of
                                 Nothing -> return ()
                                 Just h -> h
                in holding `seq` return world { _qobjects = M.insert qobject
                                                   (QObjectInfo
                                                    children
                                                    parent_ptr
                                                    holding
                                                    [finalizer])
                                                   (_qobjects world) }

data CommonQObject a = CommonQObject (IORef (Maybe (Ptr a))) !(IORef ())
                       deriving ( Eq, Typeable )

deleteCommonQObject :: CommonQObject a -> IO ()
deleteCommonQObject (CommonQObject ref _) = mask_ $
    atomicModifyIORef' ref (\old -> ( Nothing, old )) >>= \case
        Nothing -> return ()
        Just ptr -> deleteQObject (castPtr ptr)

instance Touchable (CommonQObject a) where
    touch (CommonQObject ref1 ref2) = touch ref1 >> touch ref2

class HasCommonQObject a b | a -> b where
    getCommonQObject :: a -> CommonQObject b

instance HasCommonQObject (CommonQObject a) a where
    getCommonQObject = id

castCommonQObject :: CommonQObject a -> CommonQObject b
castCommonQObject = unsafeCoerce

withCommonQObject :: HasCommonQObject c d
                  => c -> (Ptr d -> IO b) -> IO b
withCommonQObject (getCommonQObject -> CommonQObject ref touch_me) action = do
    ptr <- readIORef ref
    case ptr of
        Nothing -> error "withCommonQObject: null pointer"
        Just ptr' -> finally (action ptr') (touch touch_me)

addChild :: Ptr a -> Ptr b -> IO () -> IO ()
addChild (castPtr -> p) (castPtr -> c) !holder =
    modifyMVar_ globalQObjectWorld $ \world ->
    case M.lookup p (_qobjects world) of
        Nothing -> error "addChild: parent not in world."
        Just info -> case M.lookup c (_qobjects world) of
            Nothing -> error "addChild: child not in world."
            Just cinfo -> return world { _qobjects =
                M.adjust (parent .~ p) c $
                M.adjust (children %~ M.insert c holder) p $ (_qobjects world) }

addCommonQObject :: Ptr a -> Maybe (IORef () -> IO (IO ())) -> IO (CommonQObject a)
addCommonQObject ptr toucher_gen = mask_ $ do
    ref <- newIORef (Just ptr)
    finalizerRef <- newIORef ()
    toucher <- case toucher_gen of
        Nothing -> return Nothing
        Just fun -> Just `fmap` fun finalizerRef
    toucher `seq` addQObject (castPtr ptr) nullPtr mempty
               (writeIORef ref Nothing >> defaultFinalizer (castPtr ptr))
               toucher
    void $ mkWeakIORef finalizerRef $ deleteQObject (castPtr ptr)
    return $ CommonQObject ref finalizerRef

defaultFinalizer :: Ptr QObject -> IO ()
defaultFinalizer = post_delete_event

deleteQObject :: Ptr QObject -> IO ()
deleteQObject qobject = mask_ $ modifyMVar_ globalQObjectWorld $ \world ->
    execStateT (deleter qobject) world

deleter :: Ptr QObject -> StateT QObjectWorld IO ()
deleter qobject = do
    qobs <- use qobjects
    case M.lookup qobject qobs of
        Nothing -> error "deleteQObject: QObject not part of the world."
        Just oinfo -> do
            qobjects.at (_parent oinfo)._Just.children %= M.delete qobject
            for_ (M.keys $ _children oinfo) deleter
            for_ (_invalidators oinfo) $ \invalidator ->
                liftIO $ void (try invalidator :: IO (Either SomeException ()))
            qobjects %= M.delete qobject

