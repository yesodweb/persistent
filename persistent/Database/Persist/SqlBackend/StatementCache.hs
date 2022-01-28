{-# LANGUAGE RecordWildCards #-}
module Database.Persist.SqlBackend.StatementCache
  ( StatementCache
  , StatementCacheKey
  , mkCacheKeyFromQuery
  , MkStatementCache(..)
  , mkSimpleStatementCache
  , mkStatementCache
  ) where

import Data.Foldable
import Data.IORef
import qualified Data.Map as Map
import Database.Persist.SqlBackend.Internal.Statement
import Database.Persist.SqlBackend.Internal.StatementCache
import Data.Map (Map)
import Data.Text (Text)

-- | Configuration parameters for creating a custom statement cache
--
-- @since 2.13.X
data MkStatementCache = MkStatementCache
    { statementCacheLookup :: StatementCacheKey -> IO (Maybe Statement)
    -- ^ Retrieve a statement from the cache, or return nothing if it is not found.
    --
    -- @since 2.13.X
    , statementCacheInsert :: StatementCacheKey -> Statement -> IO ()
    -- ^ Put a new statement into the cache. An immediate lookup of
    -- the statement MUST return the inserted statement for the given
    -- cache key. Depending on the implementation, the statement cache MAY
    -- choose to evict other statements from the cache within this function.
    --
    -- @since 2.13.X
    , statementCacheClear :: IO ()
    -- ^ Remove all statements from the cache. Implementations of this
    -- should be sure to call `stmtFinalize` on all statements removed
    -- from the cache.
    --
    -- @since 2.13.X
    , statementCacheSize :: IO Int
    -- ^ Get the current size of the cache.
    --
    -- @since 2.13.X
    }


-- | Make a simple statement cache that will cache statements if they are not currently cached.
--
-- @since 2.13.X
mkSimpleStatementCache :: IORef (Map Text Statement) -> MkStatementCache
mkSimpleStatementCache stmtMap =
    MkStatementCache
        { statementCacheLookup = \sql -> Map.lookup (cacheKey sql) <$> readIORef stmtMap
        , statementCacheInsert = \sql stmt ->
            modifyIORef' stmtMap (Map.insert (cacheKey sql) stmt)
        , statementCacheClear = do
            oldStatements <- atomicModifyIORef' stmtMap (\oldStatements -> (Map.empty, oldStatements))
            traverse_ stmtFinalize oldStatements
        , statementCacheSize = Map.size <$> readIORef stmtMap
        }

-- | Create a statement cache.
--
-- @since 2.13.0
mkStatementCache :: MkStatementCache -> StatementCache
mkStatementCache MkStatementCache{..} = StatementCache { .. }
