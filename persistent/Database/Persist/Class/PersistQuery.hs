{-# LANGUAGE ExplicitForAll #-}
module Database.Persist.Class.PersistQuery
    ( selectList
    , PersistQueryRead (..)
    , PersistQueryWrite (..)
    , selectSource
    , selectKeys
    , selectKeysList
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader   (ReaderT, MonadReader)
import Control.Monad.Trans.Resource (MonadResource, release)
import Data.Acquire (Acquire, allocateAcquire, with)
import Data.Conduit (ConduitM, (.|), await, runConduit)
import qualified Data.Conduit.List as CL

import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistEntity

-- | Backends supporting conditional read operations.
class (PersistCore backend, PersistStoreRead backend) => PersistQueryRead backend where
    -- | Get all records matching the given criterion in the specified order.
    -- Returns also the identifiers.
    --
    -- NOTE: This function returns an 'Acquire' and a 'ConduitM', which implies
    -- that it streams from the database. It does not. Please use 'selectList'
    -- to simplify the code. If you want streaming behavior, consider
    -- @persistent-pagination@ which efficiently chunks a query into ranges, or
    -- investigate a backend-specific streaming solution.
    selectSourceRes
           :: (PersistRecordBackend record backend, MonadIO m1, MonadIO m2)
           => [Filter record]
           -> [SelectOpt record]
           -> ReaderT backend m1 (Acquire (ConduitM () (Entity record) m2 ()))

    -- | Get just the first record for the criterion.
    selectFirst :: (MonadIO m, PersistRecordBackend record backend)
                => [Filter record]
                -> [SelectOpt record]
                -> ReaderT backend m (Maybe (Entity record))
    selectFirst filts opts = do
        srcRes <- selectSourceRes filts (LimitTo 1 : opts)
        liftIO $ with srcRes (\src -> runConduit $ src .| await)

    -- | Get the 'Key's of all records matching the given criterion.
    selectKeysRes
        :: (MonadIO m1, MonadIO m2, PersistRecordBackend record backend)
        => [Filter record]
        -> [SelectOpt record]
        -> ReaderT backend m1 (Acquire (ConduitM () (Key record) m2 ()))

    -- | The total number of records fulfilling the given criterion.
    count :: (MonadIO m, PersistRecordBackend record backend)
          => [Filter record] -> ReaderT backend m Int

    -- | Check if there is at least one record fulfilling the given criterion.
    --
    -- @since 2.11
    exists :: (MonadIO m, PersistRecordBackend record backend)
           => [Filter record] -> ReaderT backend m Bool

-- | Backends supporting conditional write operations
class (PersistQueryRead backend, PersistStoreWrite backend) => PersistQueryWrite backend where
    -- | Update individual fields on any record matching the given criterion.
    updateWhere :: (MonadIO m, PersistRecordBackend record backend)
                => [Filter record] -> [Update record] -> ReaderT backend m ()

    -- | Delete all records matching the given criterion.
    deleteWhere :: (MonadIO m, PersistRecordBackend record backend)
                => [Filter record] -> ReaderT backend m ()

-- | Get all records matching the given criterion in the specified order.
-- Returns also the identifiers.
--
-- WARNING: This function returns a 'ConduitM', which implies that it streams
-- the results. It does not stream results on most backends. If you need
-- streaming, see @persistent-pagination@ for a means of chunking results based
-- on indexed ranges.
selectSource
       :: forall record backend m. (PersistQueryRead backend, MonadResource m, PersistRecordBackend record backend, MonadReader backend m)
       => [Filter record]
       -> [SelectOpt record]
       -> ConduitM () (Entity record) m ()
selectSource filts opts = do
    srcRes <- liftPersist $ selectSourceRes filts opts
    (releaseKey, src) <- allocateAcquire srcRes
    src
    release releaseKey

-- | Get the 'Key's of all records matching the given criterion.
--
-- For an example, see 'selectList'.
selectKeys :: forall record backend m. (PersistQueryRead backend, MonadResource m, PersistRecordBackend record backend, MonadReader backend m)
           => [Filter record]
           -> [SelectOpt record]
           -> ConduitM () (Key record) m ()
selectKeys filts opts = do
    srcRes <- liftPersist $ selectKeysRes filts opts
    (releaseKey, src) <- allocateAcquire srcRes
    src
    release releaseKey

-- | Returns a @['Entity' record]@ corresponding to the filters and options
-- provided.
--
-- Filters are constructed using the operators defined in "Database.Persist"
-- (and re-exported from "Database.Persist.Sql"). Let's look at some examples:
--
-- @
-- usersWithAgeOver40 :: 'SqlPersistT' 'IO' ['Entity' User]
-- usersWithAgeOver40 =
--     'selectList' [UserAge 'Database.Persist.>=.' 40] []
-- @
--
-- If you provide multiple values in the list, the conditions are @AND@ed
-- together.
--
-- @
-- usersWithAgeBetween30And50 :: 'SqlPersistT' 'IO' ['Entity' User]
-- usersWithAgeBetween30And50 =
--      'selectList'
--          [ UserAge 'Database.Persist.>=.' 30
--          , UserAge 'Database.Persist.<=.' 50
--          ]
--          []
-- @
--
-- The second list contains the 'SelectOpt' for a record.  We can select the
-- first ten records with 'LimitTo'
--
-- @
-- firstTenUsers =
--     'selectList' [] ['LimitTo' 10]
-- @
--
-- And we can select the second ten users with 'OffsetBy'.
--
-- @
-- secondTenUsers =
--     'selectList' [] ['LimitTo' 10, 'OffsetBy' 10]
-- @
--
-- <https://use-the-index-luke.com/sql/partial-results/fetch-next-page Warning that LIMIT/OFFSET is bad for pagination!>
--
-- With 'Asc' and 'Desc', we can provide the field we want to sort on. We can
-- provide multiple sort orders - later ones are used to sort records that are
-- equal on the first field.
--
-- @
-- newestUsers =
--     selectList [] ['Desc' UserCreatedAt, 'LimitTo' 10]
--
-- oldestUsers =
--     selectList [] ['Asc' UserCreatedAt, 'LimitTo' 10]
-- @
selectList
    :: forall record backend m. (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend)
    => [Filter record]
    -> [SelectOpt record]
    -> ReaderT backend m [Entity record]
selectList filts opts = do
    srcRes <- selectSourceRes filts opts
    liftIO $ with srcRes (\src -> runConduit $ src .| CL.consume)

-- | Call 'selectKeys' but return the result as a list.
selectKeysList :: forall record backend m. (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend)
               => [Filter record]
               -> [SelectOpt record]
               -> ReaderT backend m [Key record]
selectKeysList filts opts = do
    srcRes <- selectKeysRes filts opts
    liftIO $ with srcRes (\src -> runConduit $ src .| CL.consume)
