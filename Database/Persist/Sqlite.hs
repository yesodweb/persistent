{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Sqlite
    ( SqliteReader
    , Database
    , runSqlite
    , open
    , close
    , withSqlite
    , persistSqlite
    , Int64
    , module Database.Persist.Helper
    , persist
    ) where

import Database.Persist (Persist, Table (..), Key, Order, Filter, Update,
                         Unique, SqlType (..), PersistValue (..),
                         Persistable (..))
import Database.Persist.Helper
import Control.Monad.Trans.Reader
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Control.Monad (ap, when)
import Database.Sqlite
import Data.Int (Int64)
import Database.Persist.Quasi

persistSqlite :: [Table] -> Q [Dec]
persistSqlite = fmap concat . mapM derivePersistSqliteReader

type SqliteReader = ReaderT Database

withSqlite :: MonadCatchIO m => String -> (Database -> m a) -> m a
withSqlite s f = bracket (liftIO $ open s) (liftIO . close) f

runSqlite :: MonadCatchIO m => SqliteReader m a -> Database -> m a
runSqlite r conn = do
    Done <- liftIO begin
    res <- onException (runReaderT r conn) $ liftIO rollback
    Done <- liftIO commit
    return res
  where
    begin = bracket (prepare conn "BEGIN") finalize step
    commit = bracket (prepare conn "COMMIT") finalize step
    rollback = bracket (prepare conn "ROLLBACK") finalize step

derivePersistSqliteReader :: Table -> Q [Dec]
derivePersistSqliteReader t = do
    let name = tableName t
    let dt = dataTypeDec t
    let monad = ConT ''ReaderT `AppT` ConT ''Database
                               `AppT` VarT (mkName "m")

    fsv <- mkFromPersistValues t
    let sq =
          InstanceD [] (ConT ''FromPersistValues `AppT` ConT (mkName name))
            [ FunD (mkName "fromPersistValues") fsv
            ]

    let keysyn = TySynD (mkName $ name ++ "Id") [] $
                    ConT ''Key `AppT` ConT (mkName name)

    t' <- TH.lift t
    let mkFun s e = FunD (mkName s) [Clause [] (NormalB $ e `AppE` t') []]

    init' <- [|initialize|]
    insert' <- [|insert|]
    insertR' <- [|insertR|]
    replace' <- [|replace|]
    get' <- [|get|]
    getBy' <- [|getBy|]
    select' <- [|select|]
    deleteWhere' <- [|deleteWhere|]
    delete' <- [|delete|]
    deleteBy' <- [|deleteBy|]
    update' <- [|update|]
    updateWhere' <- [|updateWhere|]

    let inst =
          InstanceD
            [ClassP ''MonadCatchIO [VarT $ mkName "m"]]
            (ConT ''Persist `AppT` ConT (mkName name) `AppT` monad)
            [ keyTypeDec (name ++ "Id") "Int64" t
            , filterTypeDec t
            , updateTypeDec t
            , orderTypeDec t
            , uniqueTypeDec t
            , mkFun "initialize" $ init'
            , mkFun "insert" $ insert'
            , mkFun "insertR" $ insertR'
            , mkFun "replace" $ replace'
            , mkFun "get" $ get'
            , mkFun "getBy" $ getBy'
            , mkFun "select" $ select'
            , mkFun "deleteWhere" $ deleteWhere'
            , mkFun "delete" $ delete'
            , mkFun "deleteBy" $ deleteBy'
            , mkFun "update" $ update'
            , mkFun "updateWhere" $ updateWhere'
            ]

    tops <- mkToPersistables (ConT $ mkName name)
                [(name, length $ tableColumns t)]
    topsUn <- mkToPersistables (ConT ''Unique `AppT` ConT (mkName name))
            $ map (\(x, y) -> (x, length y))
            $ tableUniques t

    return
        [ dt, sq, inst, keysyn, tops, topsUn
        , mkToFieldName (ConT ''Update `AppT` ConT (mkName name))
                $ map (\s -> (name ++ upperFirst s, s))
                $ tableUpdates t
        , mkPersistable (ConT ''Update `AppT` ConT (mkName name))
                $ map (\s -> name ++ upperFirst s) $ tableUpdates t
        , mkToFieldNames (ConT ''Unique `AppT` ConT (mkName name))
                $ tableUniques t
        , mkPersistable (ConT ''Filter `AppT` ConT (mkName name))
                $ map (\(x, y) -> name ++ upperFirst x ++ y)
                $ concatMap filtsToList
                $ tableFilters t
        , mkToFieldName (ConT ''Filter `AppT` ConT (mkName name))
                $ map (\(x, y) -> (name ++ upperFirst x ++ y, x))
                $ concatMap filtsToList
                $ tableFilters t
        , mkToFilter (ConT ''Filter `AppT` ConT (mkName name))
                $ map (\(x, y) -> (name ++ upperFirst x ++ y, y))
                $ concatMap filtsToList
                $ tableFilters t
        , mkToFieldName (ConT ''Order `AppT` ConT (mkName name))
                $ map (\(x, y) -> (name ++ upperFirst x ++ y, x))
                $ concatMap ordsToList
                $ tableOrders t
        , mkToOrder (ConT ''Order `AppT` ConT (mkName name))
                $ map (\(x, y) -> (name ++ upperFirst x ++ y, y))
                $ concatMap ordsToList
                $ tableOrders t
        , mkHalfDefined (ConT $ mkName name) name $ length $ tableColumns t
        ]

initialize :: (ToPersistables v, MonadCatchIO m)
           => Table -> v -> SqliteReader m ()
initialize t v = withStmt getTableSql $ \getTable -> do
    liftIO $ bind getTable [PersistString $ tableName t]
    Row <- liftIO $ step getTable
    [PersistInt64 i] <- liftIO $ columns getTable
    when (i == 0) $ do
        let cols = zip (tableColumns t) $ toPersistables v
        let sql = "CREATE TABLE " ++ tableName t ++
                  "(id INTEGER PRIMARY KEY" ++
                  concatMap go' cols ++ ")"
        Done <- withStmt sql (liftIO . step)
        mapM_ go $ tableUniques t
        return ()
  where
    go' ((colName, (_, nullable)), p) = concat -- FIXME remove nullable
        [ ","
        , colName
        , " "
        , showSqlType $ sqlType p
        , if nullable then " NULL" else " NOT NULL" -- FIXME isNullable
        ]
    getTableSql = "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name=?"
    go (index, fields) = do
        let sql = "CREATE UNIQUE INDEX " ++ index ++ " ON " ++
                  tableName t ++ "(" ++ intercalate "," fields ++ ")"
        Done <- withStmt sql (liftIO . step)
        return ()
    showSqlType SqlString = "VARCHAR"
    showSqlType SqlInteger = "INTEGER"
    showSqlType SqlReal = "REAL"
    showSqlType SqlDay = "DATE"
    showSqlType SqlTime = "TIME"
    showSqlType SqlDayTime = "TIMESTAMP"
    showSqlType SqlBlob = "BLOB"
    showSqlType SqlBool = "BOOLEAN"

mkFromPersistValues :: Table -> Q [Clause]
mkFromPersistValues t = do
    nothing <- [|Left "Invalid fromPersistValues input"|]
    let cons = ConE $ mkName $ tableName t
    xs <- mapM (const $ newName "x") $ tableColumns t
    fs <- [|fromPersistValue|]
    let xs' = map (AppE fs . VarE) xs
    let pat = ListP $ map VarP xs
    ap' <- [|ap|]
    just <- [|Right|]
    let cons' = just `AppE` cons
    return
        [ Clause [pat] (NormalB $ foldl (go ap') cons' xs') []
        , Clause [WildP] (NormalB nothing) []
        ]
  where
    go ap' x y = InfixE (Just x) ap' (Just y)

insertReplace :: (MonadCatchIO m, ToPersistables val, Num (Key val))
              => String -> Table -> val -> SqliteReader m (Key val)
insertReplace word t val = do
    let sql = word ++ " INTO " ++ tableName t ++ " VALUES(NULL" ++
              concatMap (const ",?") (tableColumns t) ++ ")"
    withStmt sql $ \ins -> do
        liftIO $ bind ins $ toPersistValues val
        Done <- liftIO $ step ins
        withStmt "SELECT last_insert_rowid()" $ \lrow -> do
            Row <- liftIO $ step lrow
            [PersistInt64 i] <- liftIO $ columns lrow
            return $ fromIntegral i

insert :: (MonadCatchIO m, ToPersistables val, Num (Key val))
       => Table -> val -> SqliteReader m (Key val)
insert = insertReplace "INSERT"

insertR :: (MonadCatchIO m, ToPersistables val, Num (Key val))
        => Table -> val -> SqliteReader m (Key val)
insertR = insertReplace "REPLACE"

replace :: (MonadCatchIO m, Integral (Key v), ToPersistables v)
        => Table -> Key v -> v -> SqliteReader m ()
replace t k val = do
    let sql = "REPLACE INTO " ++ tableName t ++ " VALUES(?" ++
              concatMap (const ",?") (tableColumns t) ++ ")"
    withStmt sql $ \ins -> do
        liftIO $ bind ins $ PersistInt64 (fromIntegral k)
                          : map toPersistValue (toPersistables val)
        Done <- liftIO $ step ins
        return ()

get :: (Integral (Key v), MonadCatchIO m, FromPersistValues v)
    => Table -> Key v -> SqliteReader m (Maybe v)
get t k = do
    let sql = "SELECT * FROM " ++ tableName t ++ " WHERE id=?"
    withStmt sql $ \stmt -> do
        liftIO $ bind stmt [PersistInt64 $ fromIntegral k]
        res <- liftIO $ step stmt
        case res of
            Done -> return Nothing
            Row -> do
                (_:vals) <- liftIO $ columns stmt
                return $ either (const Nothing) Just $ fromPersistValues vals

select :: FromPersistValues val => Num key => MonadCatchIO m
       => Persistable (Filter val)
       => ToFieldName (Filter val) => ToFilter (Filter val)
       => ToFieldName (Order val) => ToOrder (Order val)
       => Table
       -> [Filter val]
       -> [Order val]
       -> SqliteReader m [(key, val)]
select t filts ords = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
        ord = if null ords
                then ""
                else " ORDER BY " ++
                     intercalate "," (map orderClause ords)
    let sql = "SELECT * FROM " ++ tableName t ++ wher ++ ord
    withStmt sql $ \stmt -> do
        liftIO $ bind stmt $ map toPersistValue filts
        liftIO $ go stmt id
  where
    orderClause o = toFieldName o ++ case toOrder o of
                                        Asc -> ""
                                        Desc -> " DESC"
    fromPersistValues' (PersistInt64 x:xs) = do
        xs' <- fromPersistValues xs
        return (fromIntegral x, xs')
    fromPersistValues' _ = Left "error in fromPersistValues'"
    go stmt front = do
        res <- step stmt
        case res of
            Done -> return $ front []
            Row -> do
                vals <- columns stmt
                case fromPersistValues' vals of
                    Left _ -> go stmt front
                    Right row -> go stmt $ front . (:) row

filterClause :: (ToFilter f, ToFieldName f) => f -> String
filterClause f = toFieldName f ++ showSqlFilter (toFilter f) ++ "?" -- FIXME NULL
  where
    showSqlFilter Eq = "="
    showSqlFilter Ne = "<>"
    showSqlFilter Gt = ">"
    showSqlFilter Lt = "<"
    showSqlFilter Ge = ">="
    showSqlFilter Le = "<="

delete :: (Integral (Key v), MonadCatchIO m)
       => Table -> Key v -> SqliteReader m ()
delete t k =
    withStmt ("DELETE FROM " ++ tableName t ++ " WHERE id=?") $ \del -> do
        liftIO $ bind del [PersistInt64 $ fromIntegral k]
        Done <- liftIO $ step del
        return ()

deleteWhere :: (MonadCatchIO m, Persistable (Filter v), ToFilter (Filter v),
                ToFieldName (Filter v))
            => Table -> [Filter v] -> SqliteReader m ()
deleteWhere t filts = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
        sql = "DELETE FROM " ++ tableName t ++ wher
    withStmt sql $ \del -> liftIO $ do
        bind del $ map toPersistValue filts
        Done <- step del
        return ()

deleteBy :: (MonadCatchIO m, ToPersistables (Unique v), ToFieldNames (Unique v))
         => Table -> Unique v -> SqliteReader m ()
deleteBy t uniq = do
    let sql = "DELETE FROM " ++ tableName t ++ " WHERE " ++
              intercalate " AND " (map (++ "=?") $ toFieldNames uniq)
    withStmt sql $ \del -> liftIO $ do
        bind del $ map toPersistValue $ toPersistables uniq
        Done <- step del
        return ()

update :: (Integral (Key v), MonadCatchIO m, Persistable (Update v),
           ToFieldName (Update v))
       => Table -> Key v -> [Update v] -> SqliteReader m ()
update _ _ [] = return ()
update t k upds = do
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (++ "=?") $ map toFieldName upds) ++
              " WHERE id=?"
    withStmt sql $ \up -> liftIO $ do
        bind up $ map toPersistValue upds ++
                  [PersistInt64 $ fromIntegral k]
        Done <- step up
        return ()

updateWhere :: (MonadCatchIO m, Persistable (Filter v), Persistable (Update v),
                ToFieldName (Update v), ToFilter (Filter v),
                ToFieldName (Filter v))
            => Table -> [Filter v] -> [Update v] -> SqliteReader m ()
updateWhere _ _ [] = return ()
updateWhere t filts upds = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (++ "=?") $ map toFieldName upds) ++ wher
    let dat = map toPersistValue upds ++ map toPersistValue filts
    withStmt sql $ \up -> liftIO $ do
        bind up dat
        Done <- step up
        return ()

getBy :: (Num (Key v), FromPersistValues v, MonadCatchIO m,
          ToPersistables (Unique v), ToFieldNames (Unique v))
      => Table -> Unique v -> SqliteReader m (Maybe (Key v, v))
getBy t uniq = do
    let sql = "SELECT * FROM " ++ tableName t ++ " WHERE " ++ sqlClause
    withStmt sql $ \stmt -> do
        liftIO $ bind stmt $ toPersistValues uniq
        res <- liftIO $ step stmt
        case res of
            Done -> return Nothing
            Row -> do
                PersistInt64 k:vals <- liftIO $ columns stmt
                case fromPersistValues vals of
                    Left _ -> return Nothing
                    Right vals' -> return $ Just (fromIntegral k, vals')
  where
    sqlClause = intercalate " AND " $ map (++ "=?") $ toFieldNames uniq

withStmt :: MonadCatchIO m => String -> (Statement -> SqliteReader m a)
         -> SqliteReader m a
withStmt sql f = do
    conn <- ask
    bracket (liftIO $ prepare conn sql) (liftIO . finalize) f
