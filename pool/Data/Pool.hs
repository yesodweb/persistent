{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Pool
    ( -- * Creation
      Pool
    , createPool
    , createPoolCheckAlive
      -- * Usage
    , withPool
    , withPool'
    , withPoolAllocate
      -- * Diagnostics
    , PoolStats (..)
    , poolStats
    ) where

import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Control.Exception (throwIO, Exception, bracket, finally)
import qualified Control.Exception as E
import Data.Typeable
#if MIN_VERSION_monad_control(0, 3, 0)
import qualified Control.Monad.Trans.Control as I
#else
import qualified Control.Monad.IO.Control as I
import qualified Control.Exception.Control as I
#endif
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

#if MIN_VERSION_monad_control(0, 3, 0)
#define MBCIO I.MonadBaseControl IO
#define LOO I.liftBaseOp
#define CIO I.control
#define TRY try'

sequenceEither :: I.MonadBaseControl IO m => Either e (I.StM m a) -> m (Either e a)
sequenceEither = either (return . Left) (liftM Right . I.restoreM)
{-# INLINE sequenceEither #-}

-- |Generalized version of 'E.try'.
try' :: (I.MonadBaseControl IO m, Exception e) => m a -> m (Either e a)
try' m = I.liftBaseWith (\runInIO -> E.try (runInIO m)) >>= sequenceEither
#else
#define MBCIO I.MonadControlIO
#define LOO I.liftIOOp
#define CIO I.controlIO
#define TRY I.try
#endif

-- | Create a new pool without any resource alive checking.
createPool :: (MBCIO m, MonadIO m)
           => IO a -- ^ new resource creator
           -> (a -> IO ()) -- ^ resource deallocator
           -> Int -- ^ maximum number of resources to allow in pool
           -> (Pool a -> m b) -- ^ inner function to run with the pool
           -> m b
createPool mk fr mx f = createPoolCheckAlive mk fr mx f $ const $ return True

-- | Create a new pool, including a function to check if a resource is still
-- alive. Stale resources will automatically be removed from the pool.
createPoolCheckAlive
    :: (MBCIO m, MonadIO m)
    => IO a -- ^ new resource creator
    -> (a -> IO ()) -- ^ resource deallocator
    -> Int -- ^ maximum number of resource to allow in pool
    -> (Pool a -> m b) -- ^ inner function to run with the pool
    -> (a -> IO Bool) -- ^ is the resource alive?
    -> m b
createPoolCheckAlive mk fr mx f ca = do
    pd <- liftIO $ newIORef $ PoolData [] 0
    finallyIO (f $ Pool mx pd mk fr ca) $ do
        PoolData ress _ <- readIORef pd
        mapM_ fr ress

finallyIO :: MBCIO m => m a -> IO b -> m a
finallyIO a io = CIO $ \runInIO -> finally (runInIO a) io

data PoolExhaustedException = PoolExhaustedException
    deriving (Show, Typeable)
instance Exception PoolExhaustedException

-- | This function throws a 'PoolExhaustedException' when no resources are
-- available. See 'withPoolAllocate' to avoid this.
#if MIN_VERSION_monad_control(0, 3, 0)
withPool' :: (I.MonadBaseControl IO m, MonadIO m)
#else
withPool' :: I.MonadControlIO m
#endif
          => Pool a -> (a -> m b) -> m b
withPool' p f = do
    x <- withPool p f
    case x of
        Nothing -> liftIO $ throwIO PoolExhaustedException
        Just x' -> return x'

-- | Same as @withPool'@, but instead of throwing a 'PoolExhaustedException'
-- when there the maximum number of resources are created and allocated, it
-- allocates a new resource, passes it to the subprocess and then frees it.
withPoolAllocate :: (MonadIO m, MBCIO m) => Pool a -> (a -> m b) -> m b
withPoolAllocate p f = do
    x <- withPool p f
    case x of
        Just x' -> return x'
        Nothing -> LOO (bracket (poolMake p) (poolFree p)) f

mask :: MBCIO m => ((forall a. m a -> m a) -> m b) -> m b
#if MIN_VERSION_base(4,3,0)
#if MIN_VERSION_monad_control(0, 3, 0)
mask = I.liftBaseOp E.mask . liftRestore

liftRestore :: I.MonadBaseControl IO m
            => ((forall a.  m a ->  m a) -> b)
            -> ((forall a. IO a -> IO a) -> b)
liftRestore f r = f $ I.liftBaseOp_ r
#else
mask = I.mask
#endif
#else
mask f = I.block $ f I.unblock
#endif

-- | Attempt to run the given action with a resource from the given 'Pool'.
-- Returns 'Nothing' if no resource was available.
withPool :: (MonadIO m, MBCIO m) => Pool a -> (a -> m b) -> m (Maybe b)
withPool p f = mask $ \unmask -> do
    eres <- liftIO $ atomicModifyIORef (poolData p) $ \pd ->
        case poolAvail pd of
            x:xs -> (pd { poolAvail = xs }, Right x)
            [] -> (pd, Left $ poolCreated pd)
    case eres of
        Left pc ->
            if pc >= poolMax p
                then return Nothing
                else LOO (bracket (poolMake p) (insertResource 1))
                                (liftM Just . unmask . f)
        Right res -> do
            isAlive <- TRY $ unmask $ liftIO $ poolCheckAlive p res
            case isAlive :: Either E.SomeException Bool of
                Right True -> finallyIO (liftM Just $ unmask $ f res)
                                        (insertResource 0 res)
                _ -> do
                    -- decrement the poolCreated count and then start over
                    liftIO $ atomicModifyIORef (poolData p) $ \pd ->
                        (pd { poolCreated = poolCreated pd - 1}, ())
                    unmask $ withPool p f
  where
    insertResource i x = atomicModifyIORef (poolData p) $ \pd ->
        (pd { poolAvail = x : poolAvail pd
                , poolCreated = i + poolCreated pd
                }, ())
