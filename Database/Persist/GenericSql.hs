{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | This is a helper module for creating SQL backends. Regular users do not
-- need to use this module.
module Database.Persist.GenericSql
    ( Connection (..)
    , Statement (..)
    , SqlReader (..)
    , runSqlConn
    , runSqlPool
    , withSqlConn
    , withSqlPool

    , Migration
    , runMigration
    , migrate

    , mkColumns
    , tableName
    , Column (..)
    , UniqueDef
    , RowPopper
    ) where

import Database.Persist.Base
import Data.List (intercalate)
import Control.Arrow
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import qualified Data.Map as Map
import Control.Monad.Trans.Reader
import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.IORef
import Database.Persist.Pool
import Control.Monad.Trans.Writer
import System.IO

data Connection = Connection
    { prepare :: String -> IO Statement
    , insertSql :: String -> [String] -> Either String (String, String)
    , stmtMap :: IORef (Map.Map String Statement)
    , close :: IO ()
    , migrateSql :: forall v. PersistEntity v
                 => (String -> IO Statement) -> v -> IO (Either [String] [String])
    }
data Statement = Statement
    { finalize :: IO ()
    , reset :: IO ()
    , execute :: [PersistValue] -> IO ()
    , withStmt :: forall a m. MonadCatchIO m
               => [PersistValue] -> (RowPopper m -> m a) -> m a
    }

withSqlPool :: MonadCatchIO m
            => IO Connection -> Int -> (Pool Connection -> m a) -> m a
withSqlPool mkConn = createPool mkConn close'

withSqlConn :: MonadCatchIO m => IO Connection -> (Connection -> m a) -> m a
withSqlConn open = bracket (liftIO open) (liftIO . close')

close' :: Connection -> IO ()
close' conn = do
    readIORef (stmtMap conn) >>= mapM_ finalize . Map.elems
    close conn

withStmt' :: MonadCatchIO m => String -> [PersistValue]
          -> (RowPopper (SqlReader m) -> SqlReader m a) -> SqlReader m a
withStmt' sql vals pop = do
    stmt <- getStmt sql
    ret <- withStmt stmt vals pop
    liftIO $ reset stmt
    return ret

execute' :: MonadIO m => String -> [PersistValue] -> SqlReader m ()
execute' sql vals = do
    stmt <- getStmt sql
    liftIO $ execute stmt vals
    liftIO $ reset stmt

getStmt :: MonadIO m => String -> SqlReader m Statement
getStmt sql = do
    conn <- SqlReader ask
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

newtype SqlReader m a = SqlReader (ReaderT Connection m a)
    deriving (Monad, MonadIO, MonadCatchIO, MonadTrans, Functor, Applicative)

runSqlPool :: MonadCatchIO m => SqlReader m a -> Pool Connection -> m a
runSqlPool r pconn = withPool' pconn $ runSqlConn r

runSqlConn :: MonadCatchIO m => SqlReader m a -> Connection -> m a
runSqlConn (SqlReader r) conn = runReaderT r conn

type RowPopper m = m (Maybe [PersistValue])

instance MonadCatchIO m => PersistBackend (SqlReader m) where
    insert val = do
        conn <- SqlReader ask
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

tableName :: EntityDef -> String
tableName t =
    case getSqlValue $ entityAttribs t of
        Nothing -> "tbl" ++ entityName t
        Just x -> x

toField :: (String, String, [String]) -> String
toField (n, _, as) =
    case getSqlValue as of
        Just x -> x
        Nothing -> "fld" ++ n

getFieldName :: EntityDef -> String -> String
getFieldName t s = toField $ tableColumn t s

getSqlValue :: [String] -> Maybe String
getSqlValue (('s':'q':'l':'=':x):_) = Just x
getSqlValue (_:x) = getSqlValue x
getSqlValue [] = Nothing

tableColumns :: EntityDef -> [(String, String, [String])]
tableColumns = map (\a@(_, y, z) -> (toField a, y, z)) . entityColumns

tableColumn :: EntityDef -> String -> (String, String, [String])
tableColumn t s = go $ entityColumns t
  where
    go [] = error $ "Unknown table column: " ++ s
    go ((x, y, z):rest)
        | x == s = (x, y, z)
        | otherwise = go rest

tableUniques' :: EntityDef -> [(String, [String])]
tableUniques' t = map (second $ map $ getFieldName t) $ entityUniques t

type UniqueDef = (String, [String])

-- | Create the list of columns for the given entity.
mkColumns :: PersistEntity val => val -> ([Column], [UniqueDef])
mkColumns val =
    (cols, uniqs)
  where
    colNameMap = map ((\(x, _, _) -> x) &&& toField) $ entityColumns t
    uniqs = map (second $ map $ fromJust . flip lookup colNameMap) $ entityUniques t
    cols = zipWith go (tableColumns t) $ toPersistFields $ halfDefined `asTypeOf` val
    t = entityDef val
    tn = map toLower $ tableName t
    go (name, t', as) p =
        Column name ("null" `elem` as) (sqlType p) (def as) (ref name t' as)
    def [] = Nothing
    def (('d':'e':'f':'a':'u':'l':'t':'=':d):_) = Just d
    def (_:rest) = def rest
    ref c t' [] =
        let l = length t'
            (f, b) = splitAt (l - 2) t'
         in if b == "Id"
                then Just ("tbl" ++ f, refName tn c)
                else Nothing
    ref _ _ ("noreference":_) = Nothing
    ref c _ (('r':'e':'f':'e':'r':'e':'n':'c':'e':'=':x):_) =
        Just (x, refName tn c)
    ref c x (_:y) = ref c x y

refName :: String -> String -> String
refName table column =
    map toLower table ++ '_' : map toLower column ++ "_fkey"

data Column = Column
    { cName :: String
    , _cNull :: Bool
    , _cType :: SqlType
    , _cDefault :: Maybe String
    , _cReference :: (Maybe (String, String)) -- table name, constraint name
    }

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
type Migration m = WriterT [String] (WriterT [Sql] m) ()

runMigration :: MonadCatchIO m
             => Migration (SqlReader m)
             -> SqlReader m ()
runMigration m =
    runWriterT (execWriterT m) >>= go
  where
    go ([], sql) = mapM_ execute'' sql
    go (errs, _) = error $ unlines errs
    execute'' s = do
        liftIO $ hPutStrLn stderr $ "Migrating: " ++ s
        execute' s []

migrate :: (MonadCatchIO m, PersistEntity val)
        => val
        -> Migration (SqlReader m)
migrate val = do
    conn <- lift $ lift $ SqlReader ask
    let getter = getStmt' conn
    res <- liftIO $ migrateSql conn getter val
    either tell (lift . tell) res
