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

import qualified Prelude
import Prelude hiding ((++), unlines, concat, show)
import Database.Persist.Store
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Pool
import Database.Persist.GenericSql.Internal
import Database.Persist.GenericSql.Migration
import qualified Database.Persist.GenericSql.Raw as R
import Database.Persist.GenericSql.Raw (SqlPersist (..))
#if MIN_VERSION_monad_control(0, 3, 0)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import qualified Control.Exception as E
#define MBCIO MonadBaseControl IO
#else
import Control.Monad.IO.Control (MonadControlIO)
import Control.Exception.Control (onException)

#define MBCIO MonadControlIO
#endif
import Control.Exception (throw)
import Data.Text (Text, pack, unpack, concat)
import qualified Data.Text as T
import Web.PathPieces (PathPiece (..))
import qualified Data.Text.Read
import Data.Monoid (Monoid, mappend)
import Database.Persist.EntityDef
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

type ConnectionPool = Pool Connection

instance PathPiece (Key SqlPersist entity) where
    toPathPiece (Key (PersistInt64 i)) = toPathPiece i
    toPathPiece k = throw $ PersistInvalidField $ "Invalid Key: " ++ show k
    fromPathPiece t =
        case Data.Text.Read.signed Data.Text.Read.decimal t of
            Right (i, "") -> Just $ Key $ PersistInt64 i
            _ -> Nothing

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

instance C.ResourceIO m => PersistStore SqlPersist m where
    insert val = do
        conn <- SqlPersist ask
        let esql = insertSql conn (entityDB t) (map fieldDB $ entityFields t)
        i <-
            case esql of
                Left sql -> C.runResourceT $ R.withStmt sql vals C.$$ do
                    Just [PersistInt64 i] <- CL.head
                    return i
                Right (sql1, sql2) -> do
                    execute' sql1 vals
                    C.runResourceT $ R.withStmt sql2 [] C.$$ do
                        Just [PersistInt64 i] <- CL.head
                        return i
        return $ Key $ PersistInt64 i
      where
        t = entityDef val
        vals = map toPersistValue $ toPersistFields val

    replace k val = do
        conn <- SqlPersist ask
        let t = entityDef val
        let sql = concat
                [ "UPDATE "
                , escapeName conn (entityDB t)
                , " SET "
                , T.intercalate "," (map (go conn . fieldDB) $ entityFields t)
                , " WHERE id=?"
                ]
        execute' sql $ map toPersistValue (toPersistFields val)
                       `mappend` [unKey k]
      where
        go conn x = escapeName conn x ++ "=?"

    get k = do
        conn <- SqlPersist ask
        let t = entityDef $ dummyFromKey k
        let cols = T.intercalate ","
                 $ map (escapeName conn . fieldDB) $ entityFields t
        let sql = concat
                [ "SELECT "
                , cols
                , " FROM "
                , escapeName conn $ entityDB t
                , " WHERE id=?"
                ]
        C.runResourceT $ R.withStmt sql [unKey k] C.$$ do
            res <- CL.head
            case res of
                Nothing -> return Nothing
                Just vals ->
                    case fromPersistValues vals of
                        Left e -> error $ unpack $ "get " ++ show (unKey k) ++ ": " ++ e
                        Right v -> return $ Just v

    delete k = do
        conn <- SqlPersist ask
        execute' (sql conn) [unKey k]
      where
        t = entityDef $ dummyFromKey k
        sql conn = concat
            [ "DELETE FROM "
            , escapeName conn $ entityDB t
            , " WHERE id=?"
            ]

instance C.ResourceIO m => PersistUnique SqlPersist m where
    deleteBy uniq = do
        conn <- SqlPersist ask
        execute' (sql conn) $ persistUniqueToValues uniq
      where
        t = entityDef $ dummyFromUnique uniq
        go = map snd . persistUniqueToFieldNames
        go' conn x = escapeName conn x ++ "=?"
        sql conn = concat
            [ "DELETE FROM "
            , escapeName conn $ entityDB t
            , " WHERE "
            , T.intercalate " AND " $ map (go' conn) $ go uniq
            ]

    getBy uniq = do
        conn <- SqlPersist ask
        let cols = T.intercalate "," $ (escapeName conn $ entityID t)
                 : map (escapeName conn . fieldDB) (entityFields t)
        let sql = concat
                [ "SELECT "
                , cols
                , " FROM "
                , escapeName conn $ entityDB t
                , " WHERE "
                , sqlClause conn
                ]
        C.runResourceT $ R.withStmt sql (persistUniqueToValues uniq) C.$$ do
            row <- CL.head
            case row of
                Nothing -> return Nothing
                Just (PersistInt64 k:vals) ->
                    case fromPersistValues vals of
                        Left s -> error $ unpack s
                        Right x -> return $ Just (Key $ PersistInt64 k, x)
                Just _ -> error "Database.Persist.GenericSql: Bad list in getBy"
      where
        sqlClause conn =
            T.intercalate " AND " $ map (go conn) $ toFieldNames' uniq
        go conn x = escapeName conn x ++ "=?"
        t = entityDef $ dummyFromUnique uniq
        toFieldNames' = map snd . persistUniqueToFieldNames

dummyFromKey :: Key SqlPersist v -> v 
dummyFromKey _ = error "dummyFromKey"

{- FIXME
<<<<<<< HEAD

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
      Left errs -> error $ unpack $ unlines errs
      Right sql -> return sql

printMigration :: (MBCIO m, MonadIO m) => Migration (SqlPersist m) -> SqlPersist m ()
printMigration m = do
  mig <- parseMigration' m
  mapM_ (liftIO . Data.Text.IO.putStrLn . flip snoc ';') (allSql mig)

getMigration :: (MBCIO m, MonadIO m) => Migration (SqlPersist m) -> SqlPersist m [Sql]
getMigration m = do
  mig <- parseMigration' m
  return $ allSql mig

runMigration :: (MonadIO m, MBCIO m)
             => Migration (SqlPersist m)
             -> SqlPersist m ()
runMigration m = runMigration' m False >> return ()

-- | Same as 'runMigration', but returns a list of the SQL commands executed
-- instead of printing them to stderr.
runMigrationSilent :: (MBCIO m, MonadIO m)
                   => Migration (SqlPersist m)
                   -> SqlPersist m [Text]
runMigrationSilent m = runMigration' m True

runMigration'
    :: (MBCIO m, MonadIO m)
    => Migration (SqlPersist m)
    -> Bool -- ^ is silent?
    -> SqlPersist m [Text]
runMigration' m silent = do
    mig <- parseMigration' m
    case unsafeSql mig of
        []   -> mapM (executeMigrate silent) $ safeSql mig
        errs -> error $ unpack $ concat
            [ "\n\nDatabase migration: manual intervention required.\n"
            , "The following actions are considered unsafe:\n\n"
            , unlines $ map (\s -> "    " ++ s ++ ";") $ errs
            ]

runMigrationUnsafe :: (MBCIO m, MonadIO m)
                   => Migration (SqlPersist m)
                   -> SqlPersist m ()
runMigrationUnsafe m = do
    mig <- parseMigration' m
    mapM_ (executeMigrate False) $ allSql mig

executeMigrate :: MonadIO m => Bool -> Text -> SqlPersist m Text
executeMigrate silent s = do
    unless silent $ liftIO $ hPutStrLn stderr $ "Migrating: " ++ s
    execute' s []
    return s

migrate :: (MonadIO m, MBCIO m, PersistEntity val)
        => [EntityDef]
        -> val
        -> Migration (SqlPersist m)
migrate allDefs val = do
    conn <- lift $ lift $ SqlPersist ask
    let getter = R.getStmt' conn
    res <- liftIO $ migrateSql conn allDefs getter val
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
=======
-}

dummyFromUnique :: Unique v b -> v
dummyFromUnique _ = error "dummyFromUnique"

#if MIN_VERSION_monad_control(0, 3, 0)
onException :: MonadBaseControl IO m => m α -> m β -> m α
onException m what = control $ \runInIO ->
                       E.onException (runInIO m)
                                     (runInIO what)
#endif

infixr 5 ++
(++) :: Text -> Text -> Text
(++) = mappend

show :: Show a => a -> Text
show = pack . Prelude.show
