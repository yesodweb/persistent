{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}
module Data.Pool
    ( -- * Using pools
      createPool
    , createPoolCheckAlive
    , withPool
    , withPool'
    , withPoolAllocate
    , Pool
      -- * Diagnostics
    , PoolStats (..)
    , poolStats
    ) where

import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Control.Exception (throwIO, Exception)
import qualified Control.Exception as E
import Data.Typeable
import qualified Control.Monad.IO.Control as I
import qualified Control.Exception.Control as I
import Control.Monad.IO.Class
import Control.Monad

data PoolData a = PoolData
    { poolAvail :: ![a]
    , poolCreated :: !Int
    }

data Pool a = Pool
    { poolMax :: Int
    , poolData :: IORef (PoolData a)
    , poolMake :: IO a
    , poolFree :: a -> IO ()
    , poolCheckAlive :: a -> IO Bool
    }

data PoolStats = PoolStats
    { poolStatsMax :: Int
    , poolStatsAvailable :: Int
    , poolStatsCreated :: Int
    }

poolStats :: Pool a -> IO PoolStats
poolStats p = do
    d <- readIORef $ poolData p
    return $ PoolStats (poolMax p) (length $ poolAvail d) (poolCreated d)

createPool :: (MonadIO m, I.MonadControlIO m)
           => IO a -> (a -> IO ()) -> Int -> (Pool a -> m b) -> m b
createPool mk fr mx f = createPoolCheckAlive mk fr mx f $ const $ return True

createPoolCheckAlive
    :: (MonadIO m, I.MonadControlIO m)
    => IO a -> (a -> IO ()) -> Int -> (Pool a -> m b)
    -> (a -> IO Bool) -- ^ is the resource alive?
    -> m b
createPoolCheckAlive mk fr mx f ca = do
    pd <- liftIO $ newIORef $ PoolData [] 0
    I.finally (f $ Pool mx pd mk fr ca) $ liftIO $ do
        PoolData ress _ <- readIORef pd
        mapM_ fr ress

data PoolExhaustedException = PoolExhaustedException
    deriving (Show, Typeable)
instance Exception PoolExhaustedException

-- | This function throws a 'PoolExhaustedException' when no resources are
-- available. See 'withPoolAllocate' to avoid this.
withPool' :: (MonadIO m, I.MonadControlIO m) => Pool a -> (a -> m b) -> m b
withPool' p f = do
    x <- withPool p f
    case x of
        Nothing -> liftIO $ throwIO PoolExhaustedException
        Just x' -> return x'

-- | Same as @withPool'@, but instead of throwing a 'PoolExhaustedException'
-- when there the maximum number of resources are created and allocated, it
-- allocates a new resource, passes it to the subprocess and then frees it.
withPoolAllocate :: I.MonadControlIO m => Pool a -> (a -> m b) -> m b
withPoolAllocate p f = do
    x <- withPool p f
    case x of
        Just x' -> return x'
        Nothing ->
            I.bracket
                (liftIO $ poolMake p)
                (liftIO . poolFree p)
                f

withPool :: (MonadIO m, I.MonadControlIO m)
         => Pool a -> (a -> m b) -> m (Maybe b)
withPool p f = I.mask $ \unmask -> do
    eres <- liftIO $ atomicModifyIORef (poolData p) $ \pd ->
        case poolAvail pd of
            x:xs -> (pd { poolAvail = xs }, Right x)
            [] -> (pd, Left $ poolCreated pd)
    case eres of
        Left pc ->
            if pc >= poolMax p
                then return Nothing
                else I.bracket
                    (liftIO $ poolMake p)
                    (insertResource 1)
                    (liftM Just . unmask . f)
        Right res -> do
            isAlive <- I.try $ unmask $ liftIO $ poolCheckAlive p res
            case isAlive :: Either E.SomeException Bool of
                Right True ->
                    I.finally
                        (liftM Just $ unmask $ f res)
                        (insertResource 0 res)
                _ -> do
                    -- decrement the poolCreated count and then start over
                    liftIO $ atomicModifyIORef (poolData p) $ \pd ->
                        (pd { poolCreated = poolCreated pd - 1}, ())
                    unmask $ withPool p f
  where
    insertResource i x = liftIO $ atomicModifyIORef (poolData p) $ \pd ->
        (pd { poolAvail = x : poolAvail pd
                , poolCreated = i + poolCreated pd
                }, ())
