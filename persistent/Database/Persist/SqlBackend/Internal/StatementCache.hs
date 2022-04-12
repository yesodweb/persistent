module Database.Persist.SqlBackend.Internal.StatementCache where

import Data.Text (Text)
import Database.Persist.SqlBackend.Internal.Statement

-- | A statement cache used to lookup statements that have already been prepared
-- for a given query.
--
-- @since 2.13.3
data StatementCache = StatementCache
    { statementCacheLookup :: StatementCacheKey -> IO (Maybe Statement)
    , statementCacheInsert :: StatementCacheKey -> Statement -> IO ()
    , statementCacheClear :: IO ()
    , statementCacheSize :: IO Int
    }

newtype StatementCacheKey = StatementCacheKey { cacheKey :: Text }
-- Wrapping around this to allow for more efficient keying mechanisms
-- in the future, perhaps.

-- | Construct a `StatementCacheKey` from a raw SQL query.
mkCacheKeyFromQuery :: Text -> StatementCacheKey
mkCacheKeyFromQuery = StatementCacheKey
