module Database.Persist.SqlBackend.Internal.StatementCache 
  ( StatementCache(..)
  , InternalStatementCache
  , makeSimpleStatementCache
  , internalizeStatementCache
  ) where

import Data.Foldable
import Data.IORef 
import qualified Data.Map as Map
import Data.Text (Text)
import Database.Persist.SqlBackend.Internal.Statement

class StatementCache c where
  statementCacheLookup :: c -> Text -> IO (Maybe Statement)
  statementCacheInsert :: c -> Text -> Statement -> IO ()
  statementCacheClear :: c -> IO ()
  statementCacheSize :: c -> IO Int

data InternalStatementCache = InternalStatementCache
    { _statementCacheLookup :: Text -> IO (Maybe Statement)
    , _statementCacheInsert :: Text -> Statement -> IO ()
    , _statementCacheClear :: IO ()
    , _statementCacheSize :: IO Int
    }

instance StatementCache InternalStatementCache where
  statementCacheLookup = _statementCacheLookup
  statementCacheInsert = _statementCacheInsert
  statementCacheClear = _statementCacheClear
  statementCacheSize = _statementCacheSize


internalizeStatementCache :: StatementCache c => c -> InternalStatementCache
internalizeStatementCache c = InternalStatementCache
  { _statementCacheLookup = statementCacheLookup c
  , _statementCacheInsert = statementCacheInsert c
  , _statementCacheClear = statementCacheClear c
  , _statementCacheSize = statementCacheSize c
  }

makeSimpleStatementCache :: IO InternalStatementCache
makeSimpleStatementCache = do
    stmtMap <- newIORef Map.empty
    pure $ InternalStatementCache
        { _statementCacheLookup = \sql -> Map.lookup sql <$> readIORef stmtMap
        , _statementCacheInsert = \sql stmt -> 
            modifyIORef' stmtMap (Map.insert sql stmt)
        , _statementCacheClear = do
            oldStatements <- atomicModifyIORef' stmtMap (\oldStatements -> (Map.empty, oldStatements))
            traverse_ stmtFinalize oldStatements
        , _statementCacheSize = Map.size <$> readIORef stmtMap
        }

