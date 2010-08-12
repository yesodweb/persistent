{-# LANGUAGE PackageImports #-}
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
    , dumpMigration
    , getMigration
    , runMigration
    , runMigrationUnsafe
    , migrate
    ) where

import Database.Persist.Base
import Data.List (intercalate)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (MonadTrans (..))
import Database.Persist.Pool
import Control.Monad.Trans.Writer
import System.IO
import Database.Persist.GenericSql.Internal
import qualified Database.Persist.GenericSql.Raw as R
import Database.Persist.GenericSql.Raw (SqlPersist (..))
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Control.Monad (liftM)

type ConnectionPool = Pool Connection

withStmt' :: MonadCatchIO m => String -> [PersistValue]
         -> (RowPopper (SqlPersist m) -> SqlPersist m a) -> SqlPersist m a
withStmt' = R.withStmt

execute' :: MonadIO m => String -> [PersistValue] -> SqlPersist m ()
execute' = R.execute

runSqlPool :: MonadCatchIO m => SqlPersist m a -> Pool Connection -> m a
runSqlPool r pconn = withPool' pconn $ runSqlConn r

runSqlConn :: MonadCatchIO m => SqlPersist m a -> Connection -> m a
runSqlConn (SqlPersist r) conn = do
    let getter = R.getStmt' conn
    liftIO $ begin conn getter
    x <- onException
            (runReaderT r conn)
            (liftIO $ rollback conn getter)
    liftIO $ commit conn getter
    return x

instance MonadCatchIO m => PersistBackend (SqlPersist m) where
    insert val = do
        conn <- SqlPersist ask
        let esql = insertSql conn (tableName t) (map fst3 $ tableColumns t)
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
        return $ toPersistKey i
      where
        fst3 (x, _, _) = x
        t = entityDef val
        vals = map toPersistValue $ toPersistFields val

    replace k val = do
        let t = entityDef val
        let sql = "UPDATE " ++ tableName t ++ " SET " ++
                  intercalate "," (map (go . fst3) $ tableColumns t) ++
                  " WHERE id=?"
        execute' sql $ map toPersistValue (toPersistFields val)
                       ++ [PersistInt64 $ fromPersistKey k]
      where
        go = (++ "=?")
        fst3 (x, _, _) = x

    get k = do
        let t = entityDef $ dummyFromKey k
        let cols = intercalate "," $ map (\(x, _, _) -> x) $ tableColumns t
        let sql = "SELECT " ++ cols ++ " FROM " ++
                  tableName t ++ " WHERE id=?"
        withStmt' sql [PersistInt64 $ fromPersistKey k] $ \pop -> do
            res <- pop
            case res of
                Nothing -> return Nothing
                Just vals ->
                    case fromPersistValues vals of
                        Left e -> error $ "get " ++ showPersistKey k ++ ": " ++ e
                        Right v -> return $ Just v

    count filts = do
        let wher = if null filts
                    then ""
                    else " WHERE " ++
                         intercalate " AND " (map filterClause filts)
        let sql = "SELECT COUNT(*) FROM " ++ tableName t ++ wher
        withStmt' sql (getFiltsValues filts) $ \pop -> do
            Just [PersistInt64 i] <- pop
            return $ fromIntegral i
      where
        t = entityDef $ dummyFromFilts filts

    select filts ords limit offset seed0 iter = do
        let wher = if null filts
                    then ""
                    else " WHERE " ++
                         intercalate " AND " (map filterClause filts)
            ord = if null ords
                    then ""
                    else " ORDER BY " ++
                         intercalate "," (map orderClause ords)
            lim = if limit == 0 && offset == 0
                    then ""
                    else " LIMIT " ++ show limit
            off = if offset == 0
                    then ""
                    else " OFFSET " ++ show offset
        let cols = intercalate "," $ "id"
                   : (map (\(x, _, _) -> x) $ tableColumns t)
        let sql = "SELECT " ++ cols ++ " FROM " ++ tableName t ++ wher ++ ord
                  ++ lim ++ off
        withStmt' sql (getFiltsValues filts) $ go seed0
      where
        t = entityDef $ dummyFromFilts filts
        orderClause o = getFieldName t (persistOrderToFieldName o)
                        ++ case persistOrderToOrder o of
                                            Asc -> ""
                                            Desc -> " DESC"
        fromPersistValues' (PersistInt64 x:xs) = do
            case fromPersistValues xs of
                Left e -> Left e
                Right xs' -> Right (toPersistKey x, xs')
        fromPersistValues' _ = Left "error in fromPersistValues'"
        go seed pop = do
            res <- pop
            case res of
                Nothing -> return $ Right seed
                Just vals -> do
                    case fromPersistValues' vals of
                        Left s -> error s
                        Right row -> do
                            eseed <- iter seed row
                            case eseed of
                                Left seed' -> return $ Left seed'
                                Right seed' -> go seed' pop


    selectKeys filts seed0 iter = do
        let wher = if null filts
                    then ""
                    else " WHERE " ++
                         intercalate " AND " (map filterClause filts)
        let sql = "SELECT id FROM " ++ tableName t ++ wher
        withStmt' sql (getFiltsValues filts) $ go seed0
      where
        t = entityDef $ dummyFromFilts filts
        go seed pop = do
            res <- pop
            case res of
                Nothing -> return $ Right seed
                Just [PersistInt64 i] -> do
                    eseed <- iter seed $ toPersistKey i
                    case eseed of
                        Left seed' -> return $ Left seed'
                        Right seed' -> go seed' pop
                Just y -> error $ "Unexpected in selectKeys: " ++ show y

    delete k =
        execute' sql [PersistInt64 $ fromPersistKey k]
      where
        t = entityDef $ dummyFromKey k
        sql = "DELETE FROM " ++ tableName t ++ " WHERE id=?"

    deleteWhere filts = do
        let t = entityDef $ dummyFromFilts filts
        let wher = if null filts
                    then ""
                    else " WHERE " ++
                         intercalate " AND " (map filterClause filts)
            sql = "DELETE FROM " ++ tableName t ++ wher
        execute' sql $ getFiltsValues filts

    deleteBy uniq =
        execute' sql $ persistUniqueToValues uniq
      where
        t = entityDef $ dummyFromUnique uniq
        go = map (getFieldName t) . persistUniqueToFieldNames
        sql = "DELETE FROM " ++ tableName t ++ " WHERE " ++
              intercalate " AND " (map (++ "=?") $ go uniq)

    update _ [] = return ()
    update k upds = do
        let sql = "UPDATE " ++ tableName t ++ " SET " ++
                  intercalate "," (map (++ "=?") $ map go upds) ++
                  " WHERE id=?"
        execute' sql $
            map persistUpdateToValue upds ++ [PersistInt64 $ fromPersistKey k]
      where
        t = entityDef $ dummyFromKey k
        go = getFieldName t . persistUpdateToFieldName

    updateWhere _ [] = return ()
    updateWhere filts upds = do
        let wher = if null filts
                    then ""
                    else " WHERE " ++
                         intercalate " AND " (map filterClause filts)
        let sql = "UPDATE " ++ tableName t ++ " SET " ++
                  intercalate "," (map (++ "=?") $ map go upds) ++ wher
        let dat = map persistUpdateToValue upds ++ getFiltsValues filts
        execute' sql dat
      where
        t = entityDef $ dummyFromFilts filts
        go = getFieldName t . persistUpdateToFieldName

    getBy uniq = do
        let cols = intercalate "," $ "id"
                 : (map (\(x, _, _) -> x) $ tableColumns t)
        let sql = "SELECT " ++ cols ++ " FROM " ++ tableName t ++ " WHERE " ++
                  sqlClause
        withStmt' sql (persistUniqueToValues uniq) $ \pop -> do
            row <- pop
            case row of
                Nothing -> return Nothing
                Just (PersistInt64 k:vals) ->
                    case fromPersistValues vals of
                        Left s -> error s
                        Right x -> return $ Just (toPersistKey k, x)
                Just _ -> error "Database.Persist.GenericSql: Bad list in getBy"
      where
        sqlClause = intercalate " AND " $ map (++ "=?") $ toFieldNames' uniq
        t = entityDef $ dummyFromUnique uniq
        toFieldNames' = map (getFieldName t) . persistUniqueToFieldNames

dummyFromUnique :: Unique v -> v
dummyFromUnique _ = error "dummyFromUnique"

getFieldName :: EntityDef -> String -> String
getFieldName t s = toField $ tableColumn t s

tableColumn :: EntityDef -> String -> (String, String, [String])
tableColumn t s = go $ entityColumns t
  where
    go [] = error $ "Unknown table column: " ++ s
    go ((x, y, z):rest)
        | x == s = (x, y, z)
        | otherwise = go rest

dummyFromKey :: Key v -> v
dummyFromKey _ = error "dummyFromKey"

filterClause :: PersistEntity val => Filter val -> String
filterClause f =
    case (isNull, persistFilterToFilter f) of
        (True, Eq) -> name ++ " IS NULL"
        (True, Ne) -> name ++ " IS NOT NULL"
        (False, Ne) -> concat
            [ "("
            , name
            , " IS NULL OR "
            , name
            , "<>?)"
            ]
        (False, In) -> name ++ " IN " ++ qmarks
        (True, In) -> concat
            [ "("
            , name
            , " IS NULL OR "
            , name
            , " IN "
            , qmarks
            , ")"
            ]
        (False, NotIn) -> concat
            [ "("
            , name
            , " IS NULL OR "
            , name
            , " NOT IN "
            , qmarks
            , ")"
            ]
        (True, NotIn) -> concat
            [ "("
            , name
            , " IS NOT NULL AND "
            , name
            , " NOT IN "
            , qmarks
            , ")"
            ]
        _ -> name ++ showSqlFilter (persistFilterToFilter f) ++ "?"
  where
    isNull = any (== PersistNull)
           $ either return id
           $ persistFilterToValue f
    t = entityDef $ dummyFromFilts [f]
    name = getFieldName t $ persistFilterToFieldName f
    qmarks = case persistFilterToValue f of
                Left _ -> "?"
                Right x ->
                    let x' = filter (/= PersistNull) x
                     in '(' : intercalate "," (map (const "?") x') ++ ")"
    showSqlFilter Eq = "="
    showSqlFilter Ne = "<>"
    showSqlFilter Gt = ">"
    showSqlFilter Lt = "<"
    showSqlFilter Ge = ">="
    showSqlFilter Le = "<="
    showSqlFilter In = " IN "
    showSqlFilter NotIn = " NOT IN "

dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"


type Sql = String

-- Bool indicates if the Sql is safe
type CautiousMigration = [(Bool, Sql)]
allSql :: CautiousMigration -> [Sql]
allSql = map snd
unsafeSql :: CautiousMigration -> [Sql]
unsafeSql = allSql . filter fst
safeSql :: CautiousMigration -> [Sql]
safeSql = allSql . filter (not . fst)

type Migration m = WriterT [String] (WriterT CautiousMigration m) ()

parseMigration :: Monad m => Migration m -> m (Either [String] CautiousMigration)
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
      Left errs -> error $ unlines errs
      Right sql -> return sql

dumpMigration :: MonadCatchIO m => Migration (SqlPersist m) -> SqlPersist m ()
dumpMigration m = do
  mig <- parseMigration' m
  mapM_ (liftIO . putStrLn) (allSql mig)

getMigration :: MonadCatchIO m => Migration (SqlPersist m) -> SqlPersist m [Sql]
getMigration m = do
  mig <- parseMigration' m
  return $ allSql mig

runMigration :: MonadCatchIO m
             => Migration (SqlPersist m)
             -> SqlPersist m ()
runMigration m = do
    mig <- parseMigration' m
    case unsafeSql mig of
        []   -> mapM_ executeMigrate $ safeSql mig
        errs -> error $ concat
            [ "\n\nDatabase migration: manual intervention required.\n"
            , "The following actions are considered unsafe:\n\n"
            , unlines $ map ("    " ++) $ errs
            ]

runMigrationUnsafe :: MonadCatchIO m
                   => Migration (SqlPersist m)
                   -> SqlPersist m ()
runMigrationUnsafe m = do
    mig <- parseMigration' m
    mapM_ executeMigrate $ allSql mig

executeMigrate :: MonadIO m => String -> SqlPersist m ()
executeMigrate s = do
    liftIO $ hPutStrLn stderr $ "Migrating: " ++ s
    execute' s []

migrate :: (MonadCatchIO m, PersistEntity val)
        => val
        -> Migration (SqlPersist m)
migrate val = do
    conn <- lift $ lift $ SqlPersist ask
    let getter = R.getStmt' conn
    res <- liftIO $ migrateSql conn getter val
    either tell (lift . tell) res

getFiltsValues :: PersistEntity val => [Filter val] -> [PersistValue]
getFiltsValues =
    concatMap $ go . persistFilterToValue
  where
    go (Left PersistNull) = []
    go (Left x) = [x]
    go (Right xs) = filter (/= PersistNull) xs
