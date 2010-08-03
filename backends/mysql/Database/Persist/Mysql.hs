{-# LANGUAGE PackageImports #-}
module Database.Persist.Mysql
    ( withMysqlPool
    , withMysqlConn
    , MysqlSettings (..)
    , module Database.Persist
    , module Database.Persist.GenericSql
    ) where

import Database.Persist
import Database.Persist.Base
import Database.Persist.GenericSql
import Database.Persist.GenericSql.Internal
import Database.Mysql (MysqlSettings (..))
import qualified Database.Mysql as M

import Control.Monad.IO.Class (MonadIO (..))
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Data.IORef
import qualified Data.Map as Map
import Data.List (intercalate)

withMysqlPool :: MonadCatchIO m
               => MysqlSettings
               -> Int -- ^ number of connections to open
               -> (ConnectionPool -> m a) -> m a
withMysqlPool s = withSqlPool $ open' s

withMysqlConn :: MonadCatchIO m
              => MysqlSettings -> (Connection -> m a) -> m a
withMysqlConn = withSqlConn . open'

open' :: MysqlSettings -> IO Connection
open' s = do
    conn <- M.open s
    smap <- newIORef $ Map.empty
    return Connection
        { prepare = prepare' conn
        , stmtMap = smap
        , insertSql = insertSql'
        , close = M.close conn
        , migrateSql = migrate'
        , begin = helper "BEGIN"
        , commit = helper "COMMIT"
        , rollback = helper "ROLLBACK"
        }
  where
    helper t getter = do
        stmt <- getter t
        execute stmt []

insertSql' :: String -> [String] -> Either String (String, String)
insertSql' t cols =
    Right (ins, sel)
  where
    sel = "SELECT LAST_INSERT_ID()"
    ins = concat
        [ "INSERT INTO "
        , t
        , "("
        , intercalate "," cols
        , ") VALUES("
        , intercalate "," (map (const "?") cols)
        , ")"
        ]

prepare' :: M.Connection -> String -> IO Statement
prepare' conn sql = do
    stmt <- M.prepare conn sql
    return Statement
        { finalize = M.finalize stmt
        , reset = return ()
        , execute = execute' stmt
        , withStmt = withStmt' stmt
        }

execute' = undefined
withStmt' = undefined

migrate' :: PersistEntity val
         => (String -> IO Statement)
         -> val
         -> IO (Either [String] [(Bool, String)])
migrate' = undefined
