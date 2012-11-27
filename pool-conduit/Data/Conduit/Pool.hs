-- | Allocate resources from a pool, guaranteeing resource handling via the
-- ResourceT transformer.
module Data.Conduit.Pool
    ( ManagedResource (..)
    , takeResource
    , takeResourceCheck
    , P.Pool
    , P.createPool
    , P.withResource
    , withResourceT
    ) where

import qualified Data.Pool as P
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)
import qualified Data.IORef as I

-- | The result of taking a resource.
data ManagedResource m a = ManagedResource
    { mrValue :: a -- ^ The actual resource.
    , mrReuse :: Bool -> m ()
    -- ^ Let's you specify whether the resource should be returned to the pool
    -- (via 'P.putResource') or destroyed (via 'P.destroyResource') on release.
    -- This defaults to destruction, in case of exceptions.
    , mrRelease :: m ()
    -- ^ Release this resource, either destroying it or returning it to the
    -- pool.
    }

-- | Like 'P.withResource', but uses 'MonadResource' instead of 'MonadBaseControl'.
--
-- Since 0.1.1
withResourceT :: MonadResource m => P.Pool a -> (a -> m b) -> m b
withResourceT pool f = do
    mr <- takeResource pool
    b <- f $ mrValue mr
    mrReuse mr True
    mrRelease mr
    return b

-- | Take a resource from the pool and register a release action.
takeResource :: MonadResource m => P.Pool a -> m (ManagedResource m a)
takeResource pool = do
    onRelRef <- liftIO $ I.newIORef False
    (relKey, (a, _)) <- allocate
        (P.takeResource pool)
        (\(a, local) -> do
            onRel <- I.readIORef onRelRef
            if onRel
                then P.putResource local a
                else P.destroyResource pool local a)
    return ManagedResource
        { mrValue = a
        , mrReuse = liftIO . I.writeIORef onRelRef
        , mrRelease = release relKey
        }

-- | Same as 'takeResource', but apply some action to check if a resource is
-- still valid.
takeResourceCheck :: MonadResource m
                  => P.Pool a
                  -> (a -> m Bool)
                  -> m (ManagedResource m a)
takeResourceCheck pool check = do
    mr <- takeResource pool
    isValid <- check $ mrValue mr
    if isValid
        then return mr
        else do
            mrRelease mr
            takeResourceCheck pool check
