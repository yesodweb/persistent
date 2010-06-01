{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Sqlite
    ( SqliteReader
    , runSqlite
    , open
    , close
    , withSqlite
    , persistSqlite
    ) where

import Database.Persist (Persist, Table (..), Key, Order, Filter, Update,
                         Unique, PersistValue (..), Persistable (..),
                         SqlType (..), ToPersistables (..), ToFieldNames (..),
                         FromPersistValues (..), toPersistValues, ToOrder (..),
                         PersistOrder (..), ToFieldName (..),
                         PersistFilter (..), ToFilter (..))
import Database.Persist.Helper
import Control.Monad.Trans.Reader
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Control.Monad (ap, when)
import Database.Sqlite

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
    let monad = ConT ''SqliteReader `AppT` VarT (mkName "m")

    tsv <- mkToPersistValues t
    fsv <- mkFromPersistValues t
    let sq =
          InstanceD [] (ConT ''FromPersistValues `AppT` ConT (mkName name))
            [ FunD (mkName "toPersistValues") [tsv]
            , FunD (mkName "fromPersistValues") fsv
            ]

    {-
    fc <- mkFilterClause t
    fd <- mkFilterData t
    let hf =
          InstanceD [] (ConT ''HasFilter `AppT`
                        (ConT ''Filter `AppT` ConT (mkName name)))
            [ FunD (mkName "filterClause") fc
            , FunD (mkName "filterData") fd
            ]

    oc <- mkOrderClause t
    let ho =
          InstanceD [] (ConT ''HasOrder `AppT`
                        (ConT ''Order `AppT` ConT (mkName name)))
            [FunD (mkName "orderClause") oc]

    uc <- mkUpdateClause t
    ud <- mkUpdateData t
    let hu =
          InstanceD [] (ConT ''HasUpdate `AppT`
                        (ConT ''Update `AppT` ConT (mkName name)))
            [ FunD (mkName "updateClause") uc
            , FunD (mkName "updateData") ud
            ]

    unc <- mkUniqueClause t
    und <- mkUniqueData t
    let hun =
          InstanceD [] (ConT ''HasUnique `AppT`
                        (ConT ''Unique `AppT` ConT (mkName name)))
            [ FunD (mkName "uniqueClause") unc
            , FunD (mkName "uniqueData") und
            ]

    fromsql <- [|either Left (Right . fromInteger) . safeConvert|]
    let c1 =
          InstanceD [] (ConT ''Convertible `AppT` ConT ''PersistValue `AppT`
                        (ConT ''Key `AppT` ConT (mkName name)))
            [FunD (mkName "safeConvert") [Clause [] (NormalB fromsql) []]]
    tosql <- [|Right . PersistInt64 . fromIntegral|]
    let c2 =
          InstanceD [] (ConT ''Convertible `AppT`
                        (ConT ''Key `AppT` ConT (mkName name))
                        `AppT` ConT ''PersistValue)
            [FunD (mkName "safeConvert") [Clause [] (NormalB tosql) []]]
    -}

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
    return [dt, sq, {-hf, ho, hu, hun, -} inst, {- FIXME c1, c2, -} keysyn]

initialize :: (ToPersistables v, MonadCatchIO m)
           => Table -> v -> SqliteReader m ()
initialize t v = withStmt getTableSql $ \getTable -> do
    Row <- liftIO $ step getTable
    [PersistInt64 i] <- liftIO $ columns getTable
    when (i == 1) $ do
        let cols = zip (tableColumns t) $ toPersistables v
        let sql = "CREATE TABLE " ++ tableName t ++
                  "(id INTEGER PRIMARY KEY" ++
                  concatMap go' cols ++ ")"
        Done <- withStmt sql (liftIO . step)
        mapM_ go $ tableUniques t
        return ()
  where
    go' ((colName, (_, nullable)), p) = concat
        [ colName
        , " "
        , showSqlType $ sqlType p
        , if nullable then " NULL" else " NOT NULL"
        ]
    getTableSql = "SELECT COUNT(*) FROM sqlite_master WHERE type=? AND name=?"
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

mkToPersistValues :: Table -> Q Clause
mkToPersistValues t = do
    xs <- mapM (const $ newName "x") $ tableColumns t
    ts <- [|toPersistValue|]
    let xs' = map (AppE ts . VarE) xs
    return $ Clause [ConP (mkName $ tableName t) $ map VarP xs]
                    (NormalB $ ListE xs') []

mkFromPersistValues :: Table -> Q [Clause]
mkFromPersistValues t = do
    nothing <- [|Nothing|]
    let cons = ConE $ mkName $ tableName t
    xs <- mapM (const $ newName "x") $ tableColumns t
    fs <- [|fromPersistValue|]
    let xs' = map (AppE fs . VarE) xs
    let pat = ListP $ map VarP xs
    ap' <- [|ap|]
    just <- [|Just|]
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
                return $ fromPersistValues vals

{- FIXME
degen :: [Clause] -> Q [Clause]
degen [] = do
    err <- [|error "Degenerate case, should never happen"|]
    return [Clause [WildP] (NormalB err) []]
degen x = return x
-}

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
    fromPersistValues' _ = Nothing
    go stmt front = do
        res <- step stmt
        case res of
            Done -> return $ front []
            Row -> do
                vals <- columns stmt
                let mrow = fromPersistValues' vals
                case mrow of
                    Nothing -> go stmt front
                    Just row -> go stmt $ front . (:) row

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

    {- FIXME
mkUniqueClause :: Table -> Q [Clause]
mkUniqueClause t = do error "FIXME"
    degen $ map go $ tableUniques t
  where
    go (constr, fields) =
        Clause [ConP (mkName constr) $ map (const WildP) fields]
               (NormalB $ LitE $ StringL $ intercalate " AND " $ map (++ "=?") fields) -- FIXME handle nulls
               []

mkUniqueData :: Table -> Q [Clause]
mkUniqueData t = do error "FIXME"
    ts <- [|toSql|]
    mapM (go ts) (tableUniques t) >>= degen
  where
    go ts (constr, fields) = do
        xs <- mapM (const $ newName "x") fields
        let xs' = map (AppE ts . VarE) xs
        return $ Clause [ConP (mkName constr) $ map VarP xs]
                 (NormalB $ ListE xs')
                 []
    -}

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
                    Nothing -> return Nothing
                    Just vals' -> return $ Just (fromIntegral k, vals')
  where
    sqlClause = intercalate " AND " $ map (++ "=?") $ toFieldNames uniq

withStmt :: MonadCatchIO m => String -> (Statement -> SqliteReader m a)
         -> SqliteReader m a
withStmt sql f = do
    conn <- ask
    bracket (liftIO $ prepare conn sql) (liftIO . finalize) f
