{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This is a helper module for creating SQL backends. Regular users do not
-- need to use this module.
module Database.Persist.GenericSql
    ( SqlPersist (..)
    , Connection
    , ConnectionPool
    , Statement
    , runSqlConn
    , runSqlPool
    , Key (..)

    -- * Migrations
    , Migration
    , parseMigration
    , parseMigration'
    , printMigration
    , getMigration
    , runMigration
    , runMigrationSilent
    , runMigrationUnsafe
    , migrate
    , commit
    , rollback
    ) where

import Database.Persist.Store
import Data.List (intercalate)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Pool
import Database.Persist.GenericSql.Internal
import Database.Persist.GenericSql.Migration
import qualified Database.Persist.GenericSql.Raw as R
import Database.Persist.GenericSql.Raw (SqlPersist (..))
import Control.Monad (liftM, unless)
import Data.Enumerator (Stream (..), Iteratee (..), Step (..))
#if MIN_VERSION_monad_control(0, 3, 0)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import qualified Control.Exception as E
#define MBCIO MonadBaseControl IO
#else
import Control.Monad.IO.Control (MonadControlIO)
import Control.Exception.Control (onException)

#define MBCIO MonadControlIO
#endif
import Control.Exception (throw, toException)
import Data.Text (Text, pack, unpack, snoc)
import qualified Data.Text.IO
import Web.PathPieces (SinglePiece (..))
import qualified Data.Text.Read

type ConnectionPool = Pool Connection

instance SinglePiece (Key SqlPersist entity) where
    toSinglePiece (Key (PersistInt64 i)) = toSinglePiece i
    toSinglePiece k = throw $ PersistInvalidField $ "Invalid Key: " ++ show k
    fromSinglePiece t =
        case Data.Text.Read.signed Data.Text.Read.decimal t of
            Right (i, "") -> Just $ Key $ PersistInt64 i
            _ -> Nothing

withStmt'
    :: (MBCIO m, MonadIO m)
    => Text -> [PersistValue]
    -> (RowPopper (SqlPersist m) -> SqlPersist m a) -> SqlPersist m a
withStmt' = R.withStmt

execute' :: MonadIO m => Text -> [PersistValue] -> SqlPersist m ()
execute' = R.execute

runSqlPool :: (MBCIO m, MonadIO m) => SqlPersist m a -> Pool Connection -> m a
runSqlPool r pconn = withPool' pconn $ runSqlConn r

runSqlConn :: (MBCIO m, MonadIO m) => SqlPersist m a -> Connection -> m a
runSqlConn (SqlPersist r) conn = do
    let getter = R.getStmt' conn
    liftIO $ begin conn getter
    x <- onException
            (runReaderT r conn)
            (liftIO $ rollbackC conn getter)
    liftIO $ commitC conn getter
    return x

instance (MonadIO m, MBCIO m) => PersistStore SqlPersist m where
    insert val = do
        conn <- SqlPersist ask
        let esql = insertSql conn (rawTableName t) (map fst3 $ tableColumns t)
        i <-
            case esql of
                Left sql -> withStmt' sql vals $ \pop -> do
                    Just [PersistInt64 i] <- pop
                    return i
                Right (sql1, sql2) -> do
                    execute' sql1 vals
                    withStmt' sql2 [] $ \pop -> do
                        Just [PersistInt64 i] <- pop
                        return i
        return $ Key $ PersistInt64 i
      where
        t = entityDef val
        vals = map toPersistValue $ toPersistFields val

    replace k val = do
        conn <- SqlPersist ask
        let t = entityDef val
        let sql = pack $ concat
                [ "UPDATE "
                , escapeName conn (rawTableName t)
                , " SET "
                , intercalate "," (map (go conn . fst3) $ tableColumns t)
                , " WHERE id=?"
                ]
        execute' sql $ map toPersistValue (toPersistFields val)
                       ++ [unKey k]
      where
        go conn x = escapeName conn x ++ "=?"

    get k = do
        conn <- SqlPersist ask
        let t = entityDef $ dummyFromKey k
        let cols = intercalate ","
                 $ map (\(x, _, _) -> escapeName conn x) $ tableColumns t
        let sql = pack $ concat
                [ "SELECT "
                , cols
                , " FROM "
                , escapeName conn $ rawTableName t
                , " WHERE id=?"
                ]
        withStmt' sql [unKey k] $ \pop -> do
            res <- pop
            case res of
                Nothing -> return Nothing
                Just vals ->
                    case fromPersistValues vals of
                        Left e -> error $ "get " ++ show (unKey k) ++ ": " ++ e
                        Right v -> return $ Just v

    delete k = do
        conn <- SqlPersist ask
        execute' (sql conn) [unKey k]
      where
        t = entityDef $ dummyFromKey k
        sql conn = pack $ concat
            [ "DELETE FROM "
            , escapeName conn $ rawTableName t
            , " WHERE id=?"
            ]

    deleteBy uniq = do
        conn <- SqlPersist ask
        execute' (sql conn) $ persistUniqueToValues uniq
      where
        t = entityDef $ dummyFromUnique uniq
        go = map (getFieldName t) . persistUniqueToFieldNames
        go' conn x = escapeName conn x ++ "=?"
        sql conn = pack $ concat
            [ "DELETE FROM "
            , escapeName conn $ rawTableName t
            , " WHERE "
            , intercalate " AND " $ map (go' conn) $ go uniq
            ]

    getBy uniq = do
        conn <- SqlPersist ask
        let cols = intercalate "," $ (unRawName $ rawTableIdName t)
                 : (map (\(x, _, _) -> escapeName conn x) $ tableColumns t)
        let sql = pack $ concat
                [ "SELECT "
                , cols
                , " FROM "
                , escapeName conn $ rawTableName t
                , " WHERE "
                , sqlClause conn
                ]
        withStmt' sql (persistUniqueToValues uniq) $ \pop -> do
            row <- pop
            case row of
                Nothing -> return Nothing
                Just (PersistInt64 k:vals) ->
                    case fromPersistValues vals of
                        Left s -> error s
                        Right x -> return $ Just (Key $ PersistInt64 k, x)
                Just _ -> error "Database.Persist.GenericSql: Bad list in getBy"
      where
        sqlClause conn =
            intercalate " AND " $ map (go conn) $ toFieldNames' uniq
        go conn x = escapeName conn x ++ "=?"
        t = entityDef $ dummyFromUnique uniq
        toFieldNames' = map (getFieldName t) . persistUniqueToFieldNames

dummyFromKey :: Key SqlPersist v -> v 
dummyFromKey _ = error "dummyFromKey"

dummyFromUnique :: Unique v b -> v
dummyFromUnique _ = error "dummyFromUnique"
