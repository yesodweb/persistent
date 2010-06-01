{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Sqlite
    ( SqliteReader
    , runSqlite
    , persistSqlite
    , SqlValues (..)
    , HasFilter (..)
    , HasOrder (..)
    , HasUpdate (..)
    , HasUnique (..)
    , Convertible (..)
    ) where

import Database.Persist (Persist, Table (..), Key, Order, Filter, Update, Unique)
import Database.Persist.Helper
import Control.Monad.Trans.Reader
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import Data.Maybe (mapMaybe, fromJust)
import Database.HDBC
import Database.HDBC.Sqlite3 (Connection)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Data.Convertible (Convertible (..))
import Control.Monad (ap)

persistSqlite :: [Table] -> Q [Dec]
persistSqlite = fmap concat . mapM derivePersistSqliteReader

type SqliteReader = ReaderT Connection

runSqlite :: MonadCatchIO m => SqliteReader m a -> Connection -> m a
runSqlite r conn = do
    res <- onException (runReaderT r conn) $ liftIO $ rollback conn
    liftIO $ commit conn
    return res

derivePersistSqliteReader :: Table -> Q [Dec]
derivePersistSqliteReader t = do
    let name = tableName t
    let dt = dataTypeDec t
    let monad = ConT ''SqliteReader `AppT` VarT (mkName "m")

    tsv <- mkToSqlValues t
    fsv <- mkFromSqlValues t
    let sq =
          InstanceD [] (ConT ''SqlValues `AppT` ConT (mkName name))
            [ FunD (mkName "toSqlValues") [tsv]
            , FunD (mkName "fromSqlValues") fsv
            ]

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
          InstanceD [] (ConT ''Convertible `AppT` ConT ''SqlValue `AppT`
                        (ConT ''Key `AppT` ConT (mkName name)))
            [FunD (mkName "safeConvert") [Clause [] (NormalB fromsql) []]]
    tosql <- [|Right . SqlInteger . fromIntegral|]
    let c2 =
          InstanceD [] (ConT ''Convertible `AppT`
                        (ConT ''Key `AppT` ConT (mkName name))
                        `AppT` ConT ''SqlValue)
            [FunD (mkName "safeConvert") [Clause [] (NormalB tosql) []]]

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
            [ keyTypeDec (name ++ "Id") "Integer" t
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
    return [dt, sq, hf, ho, hu, hun, inst, c1, c2, keysyn]

initialize :: MonadIO m => Table -> v -> SqliteReader m ()
initialize t _ = do
    let sql = "CREATE TABLE " ++ tableName t ++ "(id INTEGER PRIMARY KEY" ++
              concatMap (',' :) (map fst $ tableColumns t) ++ ")"
    conn <- ask
    ts <- liftIO $ getTables conn
    if tableName t `elem` ts
        then return ()
        else do
            _ <- liftIO $ run conn sql []
            liftIO $ mapM_ (go conn) $ tableUniques t
            return ()
  where
    go conn (index, fields) = do
        let sql = "CREATE UNIQUE INDEX " ++ index ++ " ON " ++
                  tableName t ++ "(" ++ intercalate "," fields ++ ")"
        _ <- run conn sql []
        return ()

class SqlValues a where
    toSqlValues :: a -> [SqlValue]
    fromSqlValues :: [SqlValue] -> Maybe a

mkToSqlValues :: Table -> Q Clause
mkToSqlValues t = do
    xs <- mapM (const $ newName "x") $ tableColumns t
    ts <- [|toSql|]
    let xs' = map (AppE ts . VarE) xs
    return $ Clause [ConP (mkName $ tableName t) $ map VarP xs]
                    (NormalB $ ListE xs') []

fromSql' :: Convertible SqlValue y => SqlValue -> Maybe y
fromSql' = either (const Nothing) Just . safeConvert

mkFromSqlValues :: Table -> Q [Clause]
mkFromSqlValues t = do
    nothing <- [|Nothing|]
    let cons = ConE $ mkName $ tableName t
    xs <- mapM (const $ newName "x") $ tableColumns t
    fs <- [|fromSql'|]
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

insertReplace :: (MonadIO m, SqlValues val, Num key)
              => String -> Table -> val -> SqliteReader m key
insertReplace word t val = do
    let sql = word ++ " INTO " ++ tableName t ++ " VALUES(NULL" ++
              concatMap (const ",?") (tableColumns t) ++ ")"
    conn <- ask
    ins <- liftIO $ prepare conn sql
    _ <- liftIO $ execute ins $ toSqlValues val
    stmt <- liftIO $ prepare conn "SELECT last_insert_rowid()"
    _ <- liftIO $ execute stmt []
    Just [i] <- liftIO $ fetchRow stmt
    return $ fromInteger $ fromSql i

insert :: (MonadIO m, SqlValues val, Num key)
       => Table -> val -> SqliteReader m key
insert = insertReplace "INSERT"

insertR :: (MonadIO m, SqlValues val, Num key)
        => Table -> val -> SqliteReader m key
insertR = insertReplace "REPLACE"

replace :: (MonadIO m, Integral (Key v), SqlValues v)
        => Table -> Key v -> v -> SqliteReader m ()
replace t k val = do
    let sql = "REPLACE INTO " ++ tableName t ++ " VALUES(?" ++
              concatMap (const ",?") (tableColumns t) ++ ")"
    conn <- ask
    ins <- liftIO $ prepare conn sql
    _ <- liftIO $ execute ins $ SqlInteger (fromIntegral k) : toSqlValues val
    return ()

get :: (Integral (Key v), MonadIO m, SqlValues v)
    => Table -> Key v -> SqliteReader m (Maybe v)
get t k = do
    let sql = "SELECT * FROM " ++ tableName t ++ " WHERE id=?"
    conn <- ask
    stmt <- liftIO $ prepare conn sql
    _ <- liftIO $ execute stmt [SqlInteger $ fromIntegral k]
    res <- liftIO $ fetchRow stmt
    case res of
        Just (_:vals) -> return $ fromSqlValues vals
        _ -> return Nothing

class HasFilter a where
    filterClause :: a -> String
    filterData :: a -> SqlValue
class HasOrder a where
    orderClause :: a -> String

degen :: [Clause] -> Q [Clause]
degen [] = do
    err <- [|error "Degenerate case, should never happen"|]
    return [Clause [WildP] (NormalB err) []]
degen x = return x

mkFilterClause :: Table -> Q [Clause]
mkFilterClause t = do
    degen $ concatMap (concatMap go . filtsToList) $ tableFilters t
  where
    go (field, comp) =
        if snd $ ty field then goNull field comp else goNotNull field comp
    goNotNull field comp =
        [Clause
          [ConP (mkName $ tableName t ++ upperFirst field ++ comp) [WildP]]
          (NormalB $ LitE $ StringL $ field ++ comp' comp ++ "?")
          []]
    goNull field comp@"Eq" =
        [ Clause
          [ConP (mkName $ tableName t ++ upperFirst field ++ comp)
                [ConP (mkName "Nothing") []]]
          (NormalB $ LitE $ StringL $ concat
            [ "(", field, comp' comp, "?", " OR ", field, " IS NULL)"
            ])
          []
        , Clause
          [ConP (mkName $ tableName t ++ upperFirst field ++ comp) [WildP]]
          (NormalB $ LitE $ StringL $ field ++ comp' comp ++ "?")
          []
        ]
    -- FIXME evil copy-and-paste
    goNull field comp@"Ne" =
        [ Clause
          [ConP (mkName $ tableName t ++ upperFirst field ++ comp)
                [ConP (mkName "Nothing") []]]
          (NormalB $ LitE $ StringL $ concat
            [ "(", field, comp' comp, "?", " OR ", field, " IS NOT NULL)"
            ])
          []
        , Clause
          [ConP (mkName $ tableName t ++ upperFirst field ++ comp) [WildP]]
          (NormalB $ LitE $ StringL $ field ++ comp' comp ++ "?")
          []
        ]
    goNull _ _ = error "goNull called badly"
    ty = fromJust . flip lookup (tableColumns t)
    comp' "Eq" = "="
    comp' "Ne" = "<>"
    comp' "Lt" = "<"
    comp' "Gt" = ">"
    comp' "Le" = "<="
    comp' "Ge" = ">="
    comp' x = error $ "Invalid comparison: " ++ x

mkFilterData :: Table -> Q [Clause]
mkFilterData t = do
    ts <- [|toSql|]
    degen $ concatMap (map (go ts) . filtsToList) $ tableFilters t
  where
    go ts (field, comp) =
        Clause [ConP (mkName $ tableName t ++ upperFirst field ++ comp) [VarP $ mkName "x"]]
               (NormalB $ ts `AppE` VarE (mkName "x"))
               []

mkOrderClause :: Table -> Q [Clause]
mkOrderClause t = do
    degen $ concatMap go $ tableOrders t
  where
    go (field, asc, desc) =
        (if asc then (:) (go' field "Asc" "") else id)
      $ (if desc then (:) (go' field "Desc" " DESC") else id) []
    go' field suf sql =
        Clause [ConP (mkName $ tableName t ++ upperFirst field ++ suf) []]
               (NormalB $ LitE $ StringL $ field ++ sql)
               []

select :: SqlValues val => Num key => MonadIO m
       => HasFilter (Filter val)
       => HasOrder (Order val)
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
    conn <- ask
    stmt <- liftIO $ prepare conn sql
    _ <- liftIO $ execute stmt $ map filterData filts
    rows <- liftIO $ fetchAllRows' stmt
    return $ mapMaybe fromSqlValues' rows
  where
    fromSqlValues' (x:xs) = do
        x' <- fromSql' x
        xs' <- fromSqlValues xs
        return (fromInteger x', xs')
    fromSqlValues' [] = Nothing

deleteWhere :: (HasFilter (Filter v), MonadIO m)
            => Table -> [Filter v] -> SqliteReader m ()
deleteWhere t filts = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
        sql = "DELETE FROM " ++ tableName t ++ wher
    conn <- ask
    del <- liftIO $ prepare conn sql
    _ <- liftIO $ execute del $ map filterData filts
    return ()

delete :: (Integral (Key v), MonadIO m)
       => Table -> Key v -> SqliteReader m ()
delete t k = do
    let sql = "DELETE FROM " ++ tableName t ++ " WHERE id=?"
    conn <- ask
    del <- liftIO $ prepare conn sql
    _ <- liftIO $ execute del [SqlInteger $ fromIntegral k]
    return ()

deleteBy :: (HasUnique (Unique v), MonadIO m)
         => Table -> Unique v -> SqliteReader m ()
deleteBy t uniq = do
    let sql = "DELETE FROM " ++ tableName t ++ " WHERE " ++ uniqueClause uniq
    conn <- ask
    del <- liftIO $ prepare conn sql
    _ <- liftIO $ execute del $ uniqueData uniq
    return ()

class HasUpdate a where
    updateClause :: a -> String
    updateData :: a -> SqlValue

mkUpdateClause :: Table -> Q [Clause]
mkUpdateClause t = do
    degen $ map go $ tableUpdates t
  where
    go field =
        Clause [ConP (mkName $ tableName t ++ upperFirst field) [WildP]]
               (NormalB $ LitE $ StringL $ field ++ "=?")
               []

mkUpdateData :: Table -> Q [Clause]
mkUpdateData t = do
    ts <- [|toSql|]
    degen $ map (go ts) $ tableUpdates t
  where
    go ts field =
        Clause [ConP (mkName $ tableName t ++ upperFirst field) [VarP $ mkName "x"]]
               (NormalB $ ts `AppE` VarE (mkName "x"))
               []

update :: (Integral (Key v), HasUpdate (Update v), MonadIO m)
       => Table -> Key v -> [Update v] -> SqliteReader m ()
update _ _ [] = return ()
update t k upds = do
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map updateClause upds) ++
              " WHERE id=?"
    conn <- ask
    up <- liftIO $ prepare conn sql
    _ <- liftIO $ execute up $ map updateData upds ++ [SqlInteger $ fromIntegral k]
    return ()

updateWhere :: (HasFilter (Filter v), HasUpdate (Update v), MonadIO m)
            => Table -> [Filter v] -> [Update v] -> SqliteReader m ()
updateWhere _ _ [] = return ()
updateWhere t filts upds = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map updateClause upds) ++ wher
    let dat = map updateData upds ++ map filterData filts
    conn <- ask
    up <- liftIO $ prepare conn sql
    _ <- liftIO $ execute up dat
    return ()

class HasUnique a where
    uniqueClause :: a -> String
    uniqueData :: a -> [SqlValue]

mkUniqueClause :: Table -> Q [Clause]
mkUniqueClause t = do
    degen $ map go $ tableUniques t
  where
    go (constr, fields) =
        Clause [ConP (mkName constr) $ map (const WildP) fields]
               (NormalB $ LitE $ StringL $ intercalate " AND " $ map (++ "=?") fields) -- FIXME handle nulls
               []

mkUniqueData :: Table -> Q [Clause]
mkUniqueData t = do
    ts <- [|toSql|]
    mapM (go ts) (tableUniques t) >>= degen
  where
    go ts (constr, fields) = do
        xs <- mapM (const $ newName "x") fields
        let xs' = map (AppE ts . VarE) xs
        return $ Clause [ConP (mkName constr) $ map VarP xs]
                 (NormalB $ ListE xs')
                 []

getBy :: (HasUnique (Unique v), Num (Key v), SqlValues v, MonadIO m)
      => Table -> Unique v -> SqliteReader m (Maybe (Key v, v))
getBy t uniq = do
    let sql = "SELECT * FROM " ++ tableName t ++ " WHERE " ++ uniqueClause uniq
    conn <- ask
    stmt <- liftIO $ prepare conn sql
    _ <- liftIO $ execute stmt $ uniqueData uniq
    res <- liftIO $ fetchRow stmt
    case res of
        Just (k:vals) -> return $ do
            k' <- fromSql' k
            vals' <- fromSqlValues vals
            return (fromInteger k', vals')
        Nothing -> return Nothing
        _ -> return Nothing
