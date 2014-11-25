{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Graphics.UI.Internal.QObject
    ( addQObject
    , clearQObjects
    , deleteQObject
    , deleteCommonQObject
    , defaultFinalizer
    , CommonQObject()
    , Holder()
    , parentKeepsAlive
    , withCommonQObject
    , addChild
    , castCommonQObject
    , HasCommonQObject(..)
    , addCommonQObject
    , removeChildIfExists
    , staysAliveByItself
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
import Data.Foldable
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Typeable
import Foreign.Ptr
import Graphics.UI.Internal.Touchable
import Graphics.UI.Internal.QTypes
import System.IO.Unsafe
import Unsafe.Coerce

foreign import ccall post_delete_event :: Ptr QObject -> IO ()

data QObjectWorld = QObjectWorld
    { _qobjects :: !(M.Map (Ptr QObject) QObjectInfo) }

data QObjectInfo = QObjectInfo
    { _children :: !(M.Map (Ptr QObject) (IO ()))
    , _debugName :: !T.Text
    , _parent :: !(Ptr QObject)
    , _holder :: !(IO ())
    , _invalidators :: [IO ()] }
makeLenses ''QObjectWorld
makeLenses ''QObjectInfo

globalQObjectWorld :: MVar QObjectWorld
globalQObjectWorld = unsafePerformIO $ newMVar $ QObjectWorld M.empty
{-# NOINLINE globalQObjectWorld #-}

clearQObjects :: IO ()
clearQObjects = mask_ $ modifyMVar_ globalQObjectWorld $ \_ ->
    return $ QObjectWorld M.empty

monitor :: IO ()
monitor = void $ forkIO $ forever $ do
    world <- (M.assocs . _qobjects) `fmap` readMVar globalQObjectWorld
    putStrLn "-start-"
    for_ world $ \(ptr, oinfo) ->
        print (ptr, _debugName oinfo, (M.keys $ _children oinfo), length (_invalidators oinfo))
    putStrLn "-end-"
    threadDelay 100000

addQObject :: Ptr QObject
           -> Ptr QObject
           -> M.Map (Ptr QObject) (IO ())
           -> IO ()
           -> Maybe (IO ())
           -> Text
           -> IO ()
addQObject qobject parent_ptr initial_children finalizer !givenholder name =
    modifyMVar_ globalQObjectWorld $ \world ->
        if M.member qobject (_qobjects world)
          then error "addQObject: QObject already part of the world."
          else let holding = case givenholder of
                                 Nothing -> return ()
                                 Just h -> h
                in holding `seq` return world { _qobjects = M.insert qobject
                                                   (QObjectInfo
                                                    initial_children
                                                    name
                                                    parent_ptr
                                                    holding
                                                    [finalizer])
                                                   (_qobjects world) }

data CommonQObject s a = CommonQObject (IORef (Maybe (Ptr a))) !(IORef ())
                         deriving ( Eq, Typeable )

deleteCommonQObject :: CommonQObject s a -> IO ()
deleteCommonQObject (CommonQObject ref _) = mask_ $
    atomicModifyIORef' ref (\old -> ( Nothing, old )) >>= \case
        Nothing -> return ()
        Just ptr -> deleteQObject (castPtr ptr)

instance Touchable (CommonQObject s a) where
    touch (CommonQObject ref1 ref2) = touch ref1 >> touch ref2

class HasCommonQObject a s b | a -> b s where
    getCommonQObject :: a -> CommonQObject s b

instance HasCommonQObject (CommonQObject s a) s a where
    getCommonQObject = id

castCommonQObject :: CommonQObject s a -> CommonQObject s b
castCommonQObject = unsafeCoerce

withCommonQObject :: HasCommonQObject c s d
                  => c -> (Ptr d -> IO b) -> IO b
withCommonQObject (getCommonQObject -> CommonQObject ref touch_me) action = do
    ptr <- readIORef ref
    case ptr of
        Nothing -> error "withCommonQObject: null pointer"
        Just ptr' -> finally (action ptr') (touch touch_me)

newtype Holder = Holder (IO ())
                 deriving ( Typeable )

parentKeepsAlive :: CommonQObject s a -> Holder
parentKeepsAlive = Holder . touch

removeChildIfExists :: Ptr a -> Ptr b -> IO ()
removeChildIfExists (castPtr -> p) (castPtr -> c) =
    modifyMVar_ globalQObjectWorld $ \world ->
    case M.lookup p (_qobjects world) of
        Nothing -> error "addChild: parent not in world."
        Just _ -> case M.lookup c (_qobjects world) of
            Nothing -> return world
            Just _ -> return world { _qobjects =
                M.adjust (parent .~ nullPtr) c $
                M.adjust (children %~ M.delete c) p $ (_qobjects world) }

addChild :: Ptr a -> Ptr b -> Holder -> IO ()
addChild (castPtr -> p) (castPtr -> c) !(Holder givenholder) =
    modifyMVar_ globalQObjectWorld $ \world ->
    case M.lookup p (_qobjects world) of
        Nothing -> error $ "addChild: parent not in world." ++ show p
        Just _ -> case M.lookup c (_qobjects world) of
            Nothing -> error $ "addChild: child not in world. " ++ show c
            Just cinfo -> return world { _qobjects =
                flip execState (_qobjects world) $ do
                    modify $ M.adjust (children %~ M.delete c) (_parent cinfo)
                    modify $ M.adjust (parent .~ p) c
                    modify $ M.adjust (children %~ M.insert c givenholder) p
                }

staysAliveByItself :: Maybe (IORef () -> IO (IO ()))
staysAliveByItself = Just $ \ref -> return $ touch ref

addCommonQObject :: Ptr a -> Maybe (IORef () -> IO (IO ())) -> Text -> IO (CommonQObject s a)
addCommonQObject ptr toucher_gen name = mask_ $ do
    ref <- newIORef (Just ptr)
    finalizerRef <- newIORef ()
    toucher <- case toucher_gen of
        Nothing -> return Nothing
        Just fun -> Just `fmap` fun finalizerRef
    toucher `seq` addQObject
                  (castPtr ptr)
                  nullPtr
                  mempty
                  (writeIORef ref Nothing >> defaultFinalizer (castPtr ptr))
                  toucher
                  name
    let common_ob = CommonQObject ref finalizerRef
    void $ mkWeakIORef finalizerRef $ deleteCommonQObject common_ob
    return common_ob

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

