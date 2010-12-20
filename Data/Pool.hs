{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}
module Data.Pool
    ( -- * Using pools
      createPool
    , withPool
    , withPool'
    , Pool
      -- * Diagnostics
    , PoolStats (..)
    , poolStats
    ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
    (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception (throwIO, Exception)
import Data.Typeable
import qualified Control.Monad.IO.Peel as I
import qualified Control.Exception.Peel as I
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

data PoolStats = PoolStats
    { poolStatsMax :: Int
    , poolStatsAvailable :: Int
    , poolStatsCreated :: Int
    }

poolStats :: Pool a -> IO PoolStats
poolStats (Pool m td _) = do
    d <- atomically $ readTVar td
    return $ PoolStats m (length $ poolAvail d) (poolCreated d)

createPool :: (MonadIO m, I.MonadPeelIO m)
           => IO a -> (a -> IO ()) -> Int -> (Pool a -> m b) -> m b
createPool mk fr mx f = do
    pd <- liftIO $ newTVarIO $ PoolData [] 0
    I.finally (f $ Pool mx pd mk) $ liftIO $ do
        PoolData ress _ <- atomically $ readTVar pd
        mapM_ fr ress

data PoolExhaustedException = PoolExhaustedException
    deriving (Show, Typeable)
instance Exception PoolExhaustedException

withPool' :: (MonadIO m, I.MonadPeelIO m) => Pool a -> (a -> m b) -> m b
withPool' p f = do
    x <- withPool p f
    case x of
        Nothing -> liftIO $ throwIO PoolExhaustedException
        Just x' -> return x'

withPool :: (MonadIO m, I.MonadPeelIO m)
         => Pool a -> (a -> m b) -> m (Maybe b)
withPool p f = I.block $ do
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
                else I.bracket
                    (liftIO $ poolMake p)
                    (insertResource 1)
                    (liftM Just . I.unblock . f)
        Right res -> I.finally
                        (liftM Just $ I.unblock $ f res)
                        (insertResource 0 res)
  where
    insertResource i x = liftIO $ atomically $ do
        pd <- readTVar $ poolData p
        writeTVar (poolData p)
            pd { poolAvail = x : poolAvail pd
               , poolCreated = i + poolCreated pd
               }
