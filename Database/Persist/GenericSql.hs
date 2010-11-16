{-# LANGUAGE TemplateHaskell #-}
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
    , printMigration
    , getMigration
    , runMigration
    , runMigrationSilent
    , runMigrationUnsafe
    , migrate
    , mkMigrate
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
import Control.Monad (liftM, unless)
import Data.Enumerator hiding (map, length)
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Monad.Invert (MonadInvertIO, onException)
import Control.Exception (toException)

type ConnectionPool = Pool Connection

withStmt' :: MonadInvertIO m => String -> [PersistValue]
         -> (RowPopper (SqlPersist m) -> SqlPersist m a) -> SqlPersist m a
withStmt' = R.withStmt

execute' :: MonadIO m => String -> [PersistValue] -> SqlPersist m ()
execute' = R.execute

runSqlPool :: MonadInvertIO m => SqlPersist m a -> Pool Connection -> m a
runSqlPool r pconn = withPool' pconn $ runSqlConn r

runSqlConn :: MonadInvertIO m => SqlPersist m a -> Connection -> m a
runSqlConn (SqlPersist r) conn = do
    let getter = R.getStmt' conn
    liftIO $ begin conn getter
    x <- onException
            (runReaderT r conn)
            (liftIO $ rollback conn getter)
    liftIO $ commit conn getter
    return x

instance MonadInvertIO m => PersistBackend (SqlPersist m) where
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
        return $ toPersistKey i
      where
        fst3 (x, _, _) = x
        t = entityDef val
        vals = map toPersistValue $ toPersistFields val

    replace k val = do
        conn <- SqlPersist ask
        let t = entityDef val
        let sql = concat
                [ "UPDATE "
                , escapeName conn (rawTableName t)
                , " SET "
                , intercalate "," (map (go conn . fst3) $ tableColumns t)
                , " WHERE id=?"
                ]
        execute' sql $ map toPersistValue (toPersistFields val)
                       ++ [PersistInt64 $ fromPersistKey k]
      where
        go conn x = escapeName conn x ++ "=?"
        fst3 (x, _, _) = x

    get k = do
        conn <- SqlPersist ask
        let t = entityDef $ dummyFromKey k
        let cols = intercalate ","
                 $ map (\(x, _, _) -> escapeName conn x) $ tableColumns t
        let sql = concat
                [ "SELECT "
                , cols
                , " FROM "
                , escapeName conn $ rawTableName t
                , " WHERE id=?"
                ]
        withStmt' sql [PersistInt64 $ fromPersistKey k] $ \pop -> do
            res <- pop
            case res of
                Nothing -> return Nothing
                Just vals ->
                    case fromPersistValues vals of
                        Left e -> error $ "get " ++ showPersistKey k ++ ": " ++ e
                        Right v -> return $ Just v

    count filts = do
        conn <- SqlPersist ask
        let wher = if null filts
                    then ""
                    else " WHERE " ++
                         intercalate " AND " (map (filterClause conn) filts)
        let sql = concat
                [ "SELECT COUNT(*) FROM "
                , escapeName conn $ rawTableName t
                , wher
                ]
        withStmt' sql (getFiltsValues filts) $ \pop -> do
            Just [PersistInt64 i] <- pop
            return $ fromIntegral i
      where
        t = entityDef $ dummyFromFilts filts

    select filts ords limit offset =
        Iteratee . start
      where
        start x = do
            conn <- SqlPersist ask
            withStmt' (sql conn) (getFiltsValues filts) $ loop x
        loop (Continue k) pop = do
            res <- pop
            case res of
                Nothing -> return $ Continue k
                Just vals -> do
                    case fromPersistValues' vals of
                        Left s -> return $ Error $ toException
                                $ PersistMarshalException s
                        Right row -> do
                            step <- runIteratee $ k $ Chunks [row]
                            loop step pop
        loop step _ = return step
        t = entityDef $ dummyFromFilts filts
        orderClause conn o =
            escapeName conn (getFieldName t $ persistOrderToFieldName o)
                        ++ case persistOrderToOrder o of
                                            Asc -> ""
                                            Desc -> " DESC"
        fromPersistValues' (PersistInt64 x:xs) = do
            case fromPersistValues xs of
                Left e -> Left e
                Right xs' -> Right (toPersistKey x, xs')
        fromPersistValues' _ = Left "error in fromPersistValues'"
        wher conn = if null filts
                    then ""
                    else " WHERE " ++
                         intercalate " AND " (map (filterClause conn) filts)
        ord conn = if null ords
                    then ""
                    else " ORDER BY " ++
                         intercalate "," (map (orderClause conn) ords)
        lim conn = case (limit, offset) of
                (0, 0) -> ""
                (0, _) -> ' ' : noLimit conn
                (_, _) -> " LIMIT " ++ show limit
        off = if offset == 0
                    then ""
                    else " OFFSET " ++ show offset
        cols conn = intercalate "," $ "id"
                   : (map (\(x, _, _) -> escapeName conn x) $ tableColumns t)
        sql conn = concat
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
            withStmt' (sql conn) (getFiltsValues filts) $ loop x
        loop (Continue k) pop = do
            res <- pop
            case res of
                Nothing -> return $ Continue k
                Just [PersistInt64 i] -> do
                    step <- runIteratee $ k $ Chunks [toPersistKey i]
                    loop step pop
                Just y -> return $ Error $ toException $ PersistMarshalException
                        $ "Unexpected in selectKeys: " ++ show y
        loop step _ = return step
        t = entityDef $ dummyFromFilts filts
        wher conn = if null filts
                    then ""
                    else " WHERE " ++
                         intercalate " AND " (map (filterClause conn) filts)
        sql conn = concat
            [ "SELECT id FROM "
            , escapeName conn $ rawTableName t
            , wher conn
            ]

    delete k = do
        conn <- SqlPersist ask
        execute' (sql conn) [PersistInt64 $ fromPersistKey k]
      where
        t = entityDef $ dummyFromKey k
        sql conn = concat
            [ "DELETE FROM "
            , escapeName conn $ rawTableName t
            , " WHERE id=?"
            ]

    deleteWhere filts = do
        conn <- SqlPersist ask
        let t = entityDef $ dummyFromFilts filts
        let wher = if null filts
                    then ""
                    else " WHERE " ++
                         intercalate " AND " (map (filterClause conn) filts)
            sql = concat
                [ "DELETE FROM "
                , escapeName conn $ rawTableName t
                , wher
                ]
        execute' sql $ getFiltsValues filts

    deleteBy uniq = do
        conn <- SqlPersist ask
        execute' (sql conn) $ persistUniqueToValues uniq
      where
        t = entityDef $ dummyFromUnique uniq
        go = map (getFieldName t) . persistUniqueToFieldNames
        go' conn x = escapeName conn x ++ "=?"
        sql conn = concat
            [ "DELETE FROM "
            , escapeName conn $ rawTableName t
            , " WHERE "
            , intercalate " AND " $ map (go' conn) $ go uniq
            ]

    update _ [] = return ()
    update k upds = do
        conn <- SqlPersist ask
        let go'' n Replace = n ++ "=?"
            go'' n Add = n ++ '=' : n ++ "+?"
            go'' n Subtract = n ++ '=' : n ++ "-?"
            go'' n Multiply = n ++ '=' : n ++ "*?"
            go'' n Divide = n ++ '=' : n ++ "/?"
        let go' (x, pu) = go'' (escapeName conn x) pu
        let sql = concat
                [ "UPDATE "
                , escapeName conn $ rawTableName t
                , " SET "
                , intercalate "," $ map (go' . go) upds
                , " WHERE id=?"
                ]
        execute' sql $
            map persistUpdateToValue upds ++ [PersistInt64 $ fromPersistKey k]
      where
        t = entityDef $ dummyFromKey k
        go x = ( getFieldName t $ persistUpdateToFieldName x
               , persistUpdateToUpdate x
               )

    updateWhere _ [] = return ()
    updateWhere filts upds = do
        conn <- SqlPersist ask
        let wher = if null filts
                    then ""
                    else " WHERE " ++
                         intercalate " AND " (map (filterClause conn) filts)
        let sql = concat
                [ "UPDATE "
                , escapeName conn $ rawTableName t
                , " SET "
                , intercalate "," $ map (go' conn . go) upds
                , wher
                ]
        let dat = map persistUpdateToValue upds ++ getFiltsValues filts
        execute' sql dat
      where
        t = entityDef $ dummyFromFilts filts
        go = getFieldName t . persistUpdateToFieldName
        go' conn x = escapeName conn x ++ "=?"

    getBy uniq = do
        conn <- SqlPersist ask
        let cols = intercalate "," $ "id"
                 : (map (\(x, _, _) -> escapeName conn x) $ tableColumns t)
        let sql = concat
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
                        Right x -> return $ Just (toPersistKey k, x)
                Just _ -> error "Database.Persist.GenericSql: Bad list in getBy"
      where
        sqlClause conn =
            intercalate " AND " $ map (go conn) $ toFieldNames' uniq
        go conn x = escapeName conn x ++ "=?"
        t = entityDef $ dummyFromUnique uniq
        toFieldNames' = map (getFieldName t) . persistUniqueToFieldNames

dummyFromUnique :: Unique v -> v
dummyFromUnique _ = error "dummyFromUnique"

getFieldName :: EntityDef -> String -> RawName
getFieldName t s = rawFieldName $ tableColumn t s

tableColumn :: EntityDef -> String -> (String, String, [String])
tableColumn t s = go $ entityColumns t
  where
    go [] = error $ "Unknown table column: " ++ s
    go ((x, y, z):rest)
        | x == s = (x, y, z)
        | otherwise = go rest

dummyFromKey :: Key v -> v
dummyFromKey _ = error "dummyFromKey"

filterClause :: PersistEntity val => Connection -> Filter val -> String
filterClause conn f =
    case (isNull, persistFilterToFilter f, varCount) of
        (True, Eq, _) -> name ++ " IS NULL"
        (True, Ne, _) -> name ++ " IS NOT NULL"
        (False, Ne, _) -> concat
            [ "("
            , name
            , " IS NULL OR "
            , name
            , "<>?)"
            ]
        -- We use 1=2 (and below 1=1) to avoid using TRUE and FALSE, since
        -- not all databases support those words directly.
        (_, In, 0) -> "1=2"
        (False, In, _) -> name ++ " IN " ++ qmarks
        (True, In, _) -> concat
            [ "("
            , name
            , " IS NULL OR "
            , name
            , " IN "
            , qmarks
            , ")"
            ]
        (_, NotIn, 0) -> "1=1"
        (False, NotIn, _) -> concat
            [ "("
            , name
            , " IS NULL OR "
            , name
            , " NOT IN "
            , qmarks
            , ")"
            ]
        (True, NotIn, _) -> concat
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
    name = escapeName conn $ getFieldName t $ persistFilterToFieldName f
    qmarks = case persistFilterToValue f of
                Left _ -> "?"
                Right x ->
                    let x' = filter (/= PersistNull) x
                     in '(' : intercalate "," (map (const "?") x') ++ ")"
    varCount = case persistFilterToValue f of
                Left _ -> 1
                Right x -> length x
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

printMigration :: MonadInvertIO m => Migration (SqlPersist m) -> SqlPersist m ()
printMigration m = do
  mig <- parseMigration' m
  mapM_ (liftIO . putStrLn) (allSql mig)

getMigration :: MonadInvertIO m => Migration (SqlPersist m) -> SqlPersist m [Sql]
getMigration m = do
  mig <- parseMigration' m
  return $ allSql mig

runMigration :: MonadInvertIO m
             => Migration (SqlPersist m)
             -> SqlPersist m ()
runMigration m = runMigration' m False >> return ()

-- | Same as 'runMigration', but returns a list of the SQL commands executed
-- instead of printing them to stderr.
runMigrationSilent :: MonadInvertIO m
                   => Migration (SqlPersist m)
                   -> SqlPersist m [String]
runMigrationSilent m = runMigration' m True

runMigration' :: MonadInvertIO m
              => Migration (SqlPersist m)
              -> Bool -- ^ is silent?
              -> SqlPersist m [String]
runMigration' m silent = do
    mig <- parseMigration' m
    case unsafeSql mig of
        []   -> mapM (executeMigrate silent) $ safeSql mig
        errs -> error $ concat
            [ "\n\nDatabase migration: manual intervention required.\n"
            , "The following actions are considered unsafe:\n\n"
            , unlines $ map (\s -> "    " ++ s ++ ";") $ errs
            ]

runMigrationUnsafe :: MonadInvertIO m
                   => Migration (SqlPersist m)
                   -> SqlPersist m ()
runMigrationUnsafe m = do
    mig <- parseMigration' m
    mapM_ (executeMigrate False) $ allSql mig

executeMigrate :: MonadIO m => Bool -> String -> SqlPersist m String
executeMigrate silent s = do
    unless silent $ liftIO $ hPutStrLn stderr $ "Migrating: " ++ s
    execute' s []
    return s

migrate :: (MonadInvertIO m, PersistEntity val)
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

-- | Creates a single function to perform all migrations for the entities
-- defined here. One thing to be aware of is dependencies: if you have entities
-- with foreign references, make sure to place those definitions after the
-- entities they reference.
mkMigrate :: String -> [EntityDef] -> Q [Dec]
mkMigrate fun defs = do
    body' <- body
    return
        [ SigD (mkName fun) typ
        , FunD (mkName fun) [Clause [] (NormalB body') []]
        ]
  where
    typ = ForallT [PlainTV $ mkName "m"]
            [ ClassP ''MonadInvertIO [VarT $ mkName "m"]
            ]
            $ ConT ''Migration `AppT` (ConT ''SqlPersist `AppT` VarT (mkName "m"))
    body =
        case defs of
            [] -> [|return ()|]
            _ -> DoE `fmap` mapM toStmt defs
    toStmt ed = do
        let n = entityName ed
        u <- [|undefined|]
        m <- [|migrate|]
        let u' = SigE u $ ConT $ mkName n
        return $ NoBindS $ m `AppE` u'
