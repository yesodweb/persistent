{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
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
    , runMigration
    , runMigrationUnsafe
    , migrate
    ) where

import Database.Persist.Base
import Data.List (intercalate)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (MonadTrans (..))
import Database.Persist.Pool
import Control.Monad.Trans.Writer
import System.IO
import Database.Persist.GenericSql.Internal
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Data.IORef
import qualified Data.Map as Map
import Control.Monad (liftM)

type ConnectionPool = Pool Connection

withStmt' :: MonadCatchIO m => String -> [PersistValue]
          -> (RowPopper (SqlPersist m) -> SqlPersist m a) -> SqlPersist m a
withStmt' sql vals pop = do
    stmt <- getStmt sql
    ret <- withStmt stmt vals pop
    liftIO $ reset stmt
    return ret

execute' :: MonadIO m => String -> [PersistValue] -> SqlPersist m ()
execute' sql vals = do
    stmt <- getStmt sql
    liftIO $ execute stmt vals
    liftIO $ reset stmt

getStmt :: MonadIO m => String -> SqlPersist m Statement
getStmt sql = do
    conn <- SqlPersist ask
    liftIO $ getStmt' conn sql

getStmt' :: Connection -> String -> IO Statement
getStmt' conn sql = do
    smap <- liftIO $ readIORef $ stmtMap conn
    case Map.lookup sql smap of
        Just stmt -> return stmt
        Nothing -> do
            stmt <- liftIO $ prepare conn sql
            liftIO $ writeIORef (stmtMap conn) $ Map.insert sql stmt smap
            return stmt

newtype SqlPersist m a = SqlPersist (ReaderT Connection m a)
    deriving (Monad, MonadIO, MonadCatchIO, MonadTrans, Functor, Applicative)

runSqlPool :: MonadCatchIO m => SqlPersist m a -> Pool Connection -> m a
runSqlPool r pconn = withPool' pconn $ runSqlConn r

runSqlConn :: MonadCatchIO m => SqlPersist m a -> Connection -> m a
runSqlConn (SqlPersist r) conn = do
    let getter = getStmt' conn
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
        withStmt' sql (map persistFilterToValue filts) $ \pop -> do
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
            lim = if limit == 0
                    then ""
                    else " LIMIT " ++ show lim
            off = if offset == 0
                    then ""
                    else " OFFSET " ++ show offset
        let cols = intercalate "," $ "id"
                   : (map (\(x, _, _) -> x) $ tableColumns t)
        let sql = "SELECT " ++ cols ++ " FROM " ++ tableName t ++ wher ++ ord
                  ++ lim ++ off
        withStmt' sql (map persistFilterToValue filts) $ go seed0
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
        execute' sql $ map persistFilterToValue filts

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
        let dat = map persistUpdateToValue upds
               ++ map persistFilterToValue filts
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
filterClause f = if persistFilterIsNull f then nullClause else mainClause
  where
    t = entityDef $ dummyFromFilts [f]
    name = getFieldName t $ persistFilterToFieldName f
    mainClause = name ++ showSqlFilter (persistFilterToFilter f) ++ "?"
    nullClause =
        case persistFilterToFilter f of
          Eq -> '(' : mainClause ++ " OR " ++ name ++ " IS NULL)"
          Ne -> '(' : mainClause ++ " OR " ++ name ++ " IS NOT NULL)"
          _ -> mainClause
    showSqlFilter Eq = "="
    showSqlFilter Ne = "<>"
    showSqlFilter Gt = ">"
    showSqlFilter Lt = "<"
    showSqlFilter Ge = ">="
    showSqlFilter Le = "<="

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

dumpMigration :: MonadCatchIO m
              => Migration (SqlPersist m)
              -> SqlPersist m ()
dumpMigration m = do
  mig <- parseMigration' m
  mapM_ (liftIO . putStrLn) (allSql mig)

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
    let getter = getStmt' conn
    res <- liftIO $ migrateSql conn getter val
    either tell (lift . tell) res
