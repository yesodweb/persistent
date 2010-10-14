{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}
module Database.Persist.Pool
    ( createPool
    , withPool
    , withPool'
    , Pool
    , withPoolF'
    , withPoolF
    , createPoolF
    ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
    (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception (throwIO)
import Data.Typeable
import "MonadCatchIO-transformers" Control.Monad.CatchIO hiding (finally)
import qualified "MonadCatchIO-transformers" Control.Monad.CatchIO as C
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
createPool = createPoolF C.finally

createPoolF :: MonadIO m
            => (m b -> m () -> m b)
            -> IO a -> (a -> IO ()) -> Int -> (Pool a -> m b) -> m b
createPoolF finally mk fr mx f = do
    pd <- liftIO $ newTVarIO $ PoolData [] 0
    finally (f $ Pool mx pd mk) $ liftIO $ do
        PoolData ress _ <- atomically $ readTVar pd
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

withPoolF' :: MonadCatchIO m
           => (m (Maybe b) -> m () -> m (Maybe b))
           -> Pool a -> (a -> m b) -> m b
withPoolF' finally p f = do
    x <- withPoolF finally p f
    case x of
        Nothing -> liftIO $ throwIO PoolExhaustedException
        Just x' -> return x'

withPool :: MonadCatchIO m => Pool a -> (a -> m b) -> m (Maybe b)
withPool = withPoolF C.finally

withPoolF :: MonadCatchIO m
          => (m (Maybe b) -> m () -> m (Maybe b))
          -> Pool a -> (a -> m b) -> m (Maybe b)
withPoolF finally p f = block $ do
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
                else do
                    x <- liftIO $ poolMake p
                    liftIO $ putStrLn "Created a new resource"
                    finally
                        (liftM Just $ unblock $ f x)
                        (liftIO (putStrLn "Re-inserted newly created resource" >> insertResource 1 x))
        Right res -> do
            liftIO $ putStrLn "Borrowed a resource from the pool"
            finally
                        (liftM Just $ unblock $ f res)
                        (liftIO (putStrLn "Returned resource to the pool") >> insertResource 0 res)
  where
    insertResource i x = liftIO $ atomically $ do
        pd <- readTVar $ poolData p
        writeTVar (poolData p)
            pd { poolAvail = x : poolAvail pd
               , poolCreated = i + poolCreated pd
               }
