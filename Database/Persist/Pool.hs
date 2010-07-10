{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}
module Database.Persist.Pool
    ( createPool
    , withPool
    , withPool'
    , Pool
    ) where

import Control.Concurrent.MVar hiding (modifyMVar, modifyMVar_)
import Control.Exception (throwIO)
import Data.Typeable
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Monad

data PoolData a = PoolData
    { poolAvail :: [a]
    , poolCreated :: Int
    }

data Pool a = Pool
    { poolMax :: Int
    , poolData :: MVar (PoolData a)
    , poolMake :: IO a
    }

createPool :: MonadCatchIO m
           => IO a -> (a -> IO ()) -> Int -> (Pool a -> m b) -> m b
createPool mk fr mx f = do
    pd <- liftIO $ newMVar $ PoolData [] 0
    finally (f $ Pool mx pd mk) $ do
        mress <- liftIO $ tryTakeMVar pd
        case mress of
            Nothing -> return ()
            Just (PoolData ress _) -> liftIO $ mapM_ fr ress

data PoolExhaustedException = PoolExhaustedException
    deriving (Show, Typeable)
instance Exception PoolExhaustedException

withPool' :: MonadCatchIO m => Pool a -> (a -> m b) -> m b
withPool' p f = do
    x <- withPool p f
    case x of
        Nothing -> liftIO $ throwIO PoolExhaustedException
        Just x' -> return x'

withPool :: MonadCatchIO m => Pool a -> (a -> m b) -> m (Maybe b)
withPool p f = block $ do
    eres <- modifyMVar (poolData p) $ \pd -> do
        case poolAvail pd of
            (x:xs) -> return (pd { poolAvail = xs }, Right x)
            [] -> return (pd, Left $ poolCreated pd)
    case eres of
        Left pc ->
            if pc >= poolMax p
                then return Nothing
                else bracket
                        (liftIO $ poolMake p)
                        (insertResource 1)
                        (liftM Just . unblock . f)
        Right res -> finally
                        (liftM Just $ unblock $ f res)
                        (insertResource 0 res)
  where
    insertResource i x = modifyMVar_ (poolData p) $ \pd ->
        return pd { poolAvail = x : poolAvail pd
                  , poolCreated = i + poolCreated pd
                  }

modifyMVar :: MonadCatchIO m => MVar a -> (a -> m (a,b)) -> m b
modifyMVar m io =
  block $ do
    a      <- liftIO $ takeMVar m
    (a',b) <- unblock (io a) `onException` liftIO (putMVar m a)
    liftIO $ putMVar m a'
    return b

modifyMVar_ :: MonadCatchIO m => MVar a -> (a -> m a) -> m ()
modifyMVar_ m io =
  block $ do
    a  <- liftIO $ takeMVar m
    a' <- unblock (io a) `onException` liftIO (putMVar m a)
    liftIO $ putMVar m a'
