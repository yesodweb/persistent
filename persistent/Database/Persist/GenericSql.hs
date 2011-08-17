{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , Key (..)
    ) where

import Database.Persist.Base
import Data.List (intercalate)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Pool
import Control.Monad.Trans.Writer
import System.IO
import Database.Persist.GenericSql.Internal
import qualified Database.Persist.GenericSql.Raw as R
import Database.Persist.GenericSql.Raw (SqlPersist (..))
import Control.Monad (liftM, unless)
import Data.Enumerator (Stream (..), Iteratee (..), Step (..))
import Control.Monad.IO.Control (MonadControlIO)
import Control.Exception.Control (onException)
import Control.Exception (toException)
import Data.Text (Text, pack, unpack, snoc)
import qualified Data.Text.IO
import Web.PathPieces (SinglePiece (..))
import qualified Data.Text.Read

type ConnectionPool = Pool Connection

instance SinglePiece (Key SqlPersist entity) where
    toSinglePiece (Key (PersistInt64 i)) = toSinglePiece i
    toSinglePiece k = error $ "Invalid SqlPersist ID: " ++ show k
    fromSinglePiece t =
        case Data.Text.Read.decimal t of
            Right (i, "") -> Just $ Key $ PersistInt64 i
            _ -> Nothing

withStmt' :: MonadControlIO m => Text -> [PersistValue]
         -> (RowPopper (SqlPersist m) -> SqlPersist m a) -> SqlPersist m a
withStmt' = R.withStmt

execute' :: MonadIO m => Text -> [PersistValue] -> SqlPersist m ()
execute' = R.execute

runSqlPool :: MonadControlIO m => SqlPersist m a -> Pool Connection -> m a
runSqlPool r pconn = withPool' pconn $ runSqlConn r

runSqlConn :: MonadControlIO m => SqlPersist m a -> Connection -> m a
runSqlConn (SqlPersist r) conn = do
    let getter = R.getStmt' conn
    liftIO $ begin conn getter
    x <- onException
            (runReaderT r conn)
            (liftIO $ rollbackC conn getter)
    liftIO $ commitC conn getter
    return x

instance MonadControlIO m => PersistBackend SqlPersist m where
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

    count filts = do
        conn <- SqlPersist ask
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
        let sql = pack $ concat
                [ "SELECT COUNT(*) FROM "
                , escapeName conn $ rawTableName t
                , wher
                ]
        withStmt' sql (getFiltsValues conn filts) $ \pop -> do
            Just [PersistInt64 i] <- pop
            return $ fromIntegral i
      where
        t = entityDef $ dummyFromFilts filts

    selectEnum filts opts =
        Iteratee . start
      where
        limit  = fst3 $ limitOffsetOrder opts
        offset = snd3 $ limitOffsetOrder opts
        orders = third3 $ limitOffsetOrder opts

        start x = do
            conn <- SqlPersist ask
            withStmt' (sql conn) (getFiltsValues conn filts) $ loop x
        loop (Continue k) pop = do
            res <- pop
            case res of
                Nothing -> return $ Continue k
                Just vals -> do
                    case fromPersistValues' vals of
                        Left s -> return $ Error $ toException
                                $ PersistMarshalError s
                        Right row -> do
                            step <- runIteratee $ k $ Chunks [row]
                            loop step pop
        loop step _ = return step
        t = entityDef $ dummyFromFilts filts
        fromPersistValues' (PersistInt64 x:xs) = do
            case fromPersistValues xs of
                Left e -> Left e
                Right xs' -> Right (Key $ PersistInt64 x, xs')
        fromPersistValues' _ = Left "error in fromPersistValues'"
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        ord conn =
            case map (orderClause False conn) orders of
                [] -> ""
                ords -> " ORDER BY " ++ intercalate "," ords
        lim conn = case (limit, offset) of
                (0, 0) -> ""
                (0, _) -> ' ' : noLimit conn
                (_, _) -> " LIMIT " ++ show limit
        off = if offset == 0
                    then ""
                    else " OFFSET " ++ show offset
        cols conn = intercalate "," $ "id"
                   : (map (\(x, _, _) -> escapeName conn x) $ tableColumns t)
        sql conn = pack $ concat
            [ "SELECT "
            , cols conn
            , " FROM "
            , escapeName conn $ rawTableName t
            , wher conn
            , ord conn
            , lim conn
            , off
            ]

    selectKeys filts =
        Iteratee . start
      where
        start x = do
            conn <- SqlPersist ask
            withStmt' (sql conn) (getFiltsValues conn filts) $ loop x
        loop (Continue k) pop = do
            res <- pop
            case res of
                Nothing -> return $ Continue k
                Just [PersistInt64 i] -> do
                    step <- runIteratee $ k $ Chunks [Key $ PersistInt64 i]
                    loop step pop
                Just y -> return $ Error $ toException $ PersistMarshalError
                        $ "Unexpected in selectKeys: " ++ show y
        loop step _ = return step
        t = entityDef $ dummyFromFilts filts
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        sql conn = pack $ concat
            [ "SELECT id FROM "
            , escapeName conn $ rawTableName t
            , wher conn
            ]

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

    deleteWhere filts = do
        conn <- SqlPersist ask
        let t = entityDef $ dummyFromFilts filts
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
            sql = pack $ concat
                [ "DELETE FROM "
                , escapeName conn $ rawTableName t
                , wher
                ]
        execute' sql $ getFiltsValues conn filts

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

    update _ [] = return ()
    update k upds = do
        conn <- SqlPersist ask
        let go'' n Assign = n ++ "=?"
            go'' n Add = n ++ '=' : n ++ "+?"
            go'' n Subtract = n ++ '=' : n ++ "-?"
            go'' n Multiply = n ++ '=' : n ++ "*?"
            go'' n Divide = n ++ '=' : n ++ "/?"
        let go' (x, pu) = go'' (escapeName conn x) pu
        let sql = pack $ concat
                [ "UPDATE "
                , escapeName conn $ rawTableName t
                , " SET "
                , intercalate "," $ map (go' . go) upds
                , " WHERE id=?"
                ]
        execute' sql $
            map updatePersistValue upds ++ [unKey k]
      where
        t = entityDef $ dummyFromKey k
        go x = ( getFieldName t $ updateFieldName x
               , updateUpdate x
               )

    updateWhere _ [] = return ()
    updateWhere filts upds = do
        conn <- SqlPersist ask
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
        let sql = pack $ concat
                [ "UPDATE "
                , escapeName conn $ rawTableName t
                , " SET "
                , intercalate "," $ map (go' conn . go) upds
                , wher
                ]
        let dat = map updatePersistValue upds ++ getFiltsValues conn filts
        execute' sql dat
      where
        t = entityDef $ dummyFromFilts filts
        go'' n Assign = n ++ "=?"
        go'' n Add = n ++ '=' : n ++ "+?"
        go'' n Subtract = n ++ '=' : n ++ "-?"
        go'' n Multiply = n ++ '=' : n ++ "*?"
        go'' n Divide = n ++ '=' : n ++ "/?"
        go' conn (x, pu) = go'' (escapeName conn x) pu
        go x = ( getFieldName t $ updateFieldName x
               , updateUpdate x
               )

    getBy uniq = do
        conn <- SqlPersist ask
        let cols = intercalate "," $ "id"
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

dummyFromUnique :: Unique v b -> v
dummyFromUnique _ = error "dummyFromUnique"

dummyFromKey :: Key SqlPersist v -> v
dummyFromKey _ = error "dummyFromKey"


type Sql = Text

-- Bool indicates if the Sql is safe
type CautiousMigration = [(Bool, Sql)]
allSql :: CautiousMigration -> [Sql]
allSql = map snd
unsafeSql :: CautiousMigration -> [Sql]
unsafeSql = allSql . filter fst
safeSql :: CautiousMigration -> [Sql]
safeSql = allSql . filter (not . fst)

type Migration m = WriterT [Text] (WriterT CautiousMigration m) ()

parseMigration :: Monad m => Migration m -> m (Either [Text] CautiousMigration)
parseMigration =
    liftM go . runWriterT . execWriterT
  where
    go ([], sql) = Right sql
    go (errs, _) = Left errs

-- like parseMigration, but call error or return the CautiousMigration
parseMigration' :: Monad m => Migration m -> m (CautiousMigration)
parseMigration' m = do
  x <- parseMigration m
  case x of
      Left errs -> error $ unlines $ map unpack errs
      Right sql -> return sql

printMigration :: MonadControlIO m => Migration (SqlPersist m) -> SqlPersist m ()
printMigration m = do
  mig <- parseMigration' m
  mapM_ (liftIO . Data.Text.IO.putStrLn . flip snoc ';') (allSql mig)

getMigration :: MonadControlIO m => Migration (SqlPersist m) -> SqlPersist m [Sql]
getMigration m = do
  mig <- parseMigration' m
  return $ allSql mig

runMigration :: MonadControlIO m
             => Migration (SqlPersist m)
             -> SqlPersist m ()
runMigration m = runMigration' m False >> return ()

-- | Same as 'runMigration', but returns a list of the SQL commands executed
-- instead of printing them to stderr.
runMigrationSilent :: MonadControlIO m
                   => Migration (SqlPersist m)
                   -> SqlPersist m [Text]
runMigrationSilent m = runMigration' m True

runMigration' :: MonadControlIO m
              => Migration (SqlPersist m)
              -> Bool -- ^ is silent?
              -> SqlPersist m [Text]
runMigration' m silent = do
    mig <- parseMigration' m
    case unsafeSql mig of
        []   -> mapM (executeMigrate silent) $ safeSql mig
        errs -> error $ concat
            [ "\n\nDatabase migration: manual intervention required.\n"
            , "The following actions are considered unsafe:\n\n"
            , unlines $ map (\s -> "    " ++ unpack s ++ ";") $ errs
            ]

runMigrationUnsafe :: MonadControlIO m
                   => Migration (SqlPersist m)
                   -> SqlPersist m ()
runMigrationUnsafe m = do
    mig <- parseMigration' m
    mapM_ (executeMigrate False) $ allSql mig

executeMigrate :: MonadIO m => Bool -> Text -> SqlPersist m Text
executeMigrate silent s = do
    unless silent $ liftIO $ hPutStrLn stderr $ "Migrating: " ++ unpack s
    execute' s []
    return s

migrate :: (MonadControlIO m, PersistEntity val)
        => val
        -> Migration (SqlPersist m)
migrate val = do
    conn <- lift $ lift $ SqlPersist ask
    let getter = R.getStmt' conn
    res <- liftIO $ migrateSql conn getter val
    either tell (lift . tell) res

updatePersistValue :: Update v -> PersistValue
updatePersistValue (Update _ v _) = toPersistValue v

-- | Perform a database commit.
commit :: MonadIO m => SqlPersist m ()
commit = do
    conn <- SqlPersist ask
    let getter = R.getStmt' conn
    liftIO $ commitC conn getter >> begin conn getter

-- | Perform a database rollback.
rollback :: MonadIO m => SqlPersist m ()
rollback = do
    conn <- SqlPersist ask
    let getter = R.getStmt' conn
    liftIO $ rollbackC conn getter >> begin conn getter
