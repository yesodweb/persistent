{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}
module Database.Persist.Pool
    ( createPool
    , withPool
    , withPool'
    , Pool
    ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
    (TVar, newTVarIO, readTVar, writeTVar, readTVarIO)
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
    , poolData :: TVar (PoolData a)
    , poolMake :: IO a
    }

createPool :: MonadCatchIO m
           => IO a -> (a -> IO ()) -> Int -> (Pool a -> m b) -> m b
createPool mk fr mx f = do
    pd <- liftIO $ newTVarIO $ PoolData [] 0
    finally (f $ Pool mx pd mk) $ liftIO $ do
        PoolData ress _ <- readTVarIO pd
        mapM_ fr ress

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
    eres <- liftIO $ atomically $ do
        pd <- readTVar $ poolData p
        let (pd', eres) =
                case poolAvail pd of
                    (x:xs) -> (pd { poolAvail = xs }, Right x)
                    [] -> (pd, Left $ poolCreated pd)
        writeTVar (poolData p) pd'
        return eres
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
    insertResource i x = liftIO $ atomically $ do
        pd <- readTVar $ poolData p
        writeTVar (poolData p)
            pd { poolAvail = x : poolAvail pd
               , poolCreated = i + poolCreated pd
               }
