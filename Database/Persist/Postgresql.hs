{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Postgresql
    ( PostgresqlReader
    , Connection
    , runPostgresql
    , persistPostgresql
    , Int64
    , module Database.Persist.Helper
    , persist
    , connectPostgreSQL
    ) where

import Database.Persist (Persist, Table, Key, Order, Filter, Update,
                         Unique, SqlType (..), PersistValue (..),
                         Persistable (..))
import qualified Database.Persist as P
import Database.Persist.Helper
import Control.Monad.Trans.Reader
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Control.Monad (unless)
import qualified Database.HDBC as H
import Database.HDBC.PostgreSQL
import Data.Int (Int64)
import Database.Persist.Quasi
import Data.Char (toLower)
import Control.Arrow (first, second)

persistPostgresql :: [Table] -> Q [Dec]
persistPostgresql = fmap concat . mapM derivePersistPostgresqlReader

type PostgresqlReader = ReaderT Connection

runPostgresql :: MonadCatchIO m => PostgresqlReader m a -> Connection -> m a
runPostgresql r conn = do
    res <- onException (runReaderT r conn) $ liftIO (H.rollback conn)
    liftIO $ H.commit conn
    return res

derivePersistPostgresqlReader :: Table -> Q [Dec]
derivePersistPostgresqlReader t = do
    let name = P.tableName t
    let dt = dataTypeDec t
    let monad = ConT ''ReaderT `AppT` ConT ''Connection
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
            $ P.tableUniques t

    return
        [ dt, sq, inst, keysyn, tops, topsUn
        , mkToFieldName (ConT ''Update `AppT` ConT (mkName name))
                $ map (\s -> (name ++ upperFirst s, s))
                $ P.tableUpdates t
        , mkPersistable (ConT ''Update `AppT` ConT (mkName name))
                $ map (\s -> name ++ upperFirst s) $ P.tableUpdates t
        , mkToFieldNames (ConT ''Unique `AppT` ConT (mkName name))
                $ P.tableUniques t
        , mkPersistable (ConT ''Filter `AppT` ConT (mkName name))
                $ map (\(x, y) -> name ++ upperFirst x ++ y)
                $ concatMap filtsToList
                $ P.tableFilters t
        , mkToFieldName (ConT ''Filter `AppT` ConT (mkName name))
                $ map (\(x, y) -> (name ++ upperFirst x ++ y, x))
                $ concatMap filtsToList
                $ P.tableFilters t
        , mkToFilter (ConT ''Filter `AppT` ConT (mkName name))
                $ map (addIsNullable $ tableColumns t)
                $ map (\(x, y) -> (x, (name ++ upperFirst x ++ y, y)))
                $ concatMap filtsToList
                $ P.tableFilters t
        , mkToFieldName (ConT ''Order `AppT` ConT (mkName name))
                $ map (\(x, y) -> (name ++ upperFirst x ++ y, x))
                $ concatMap ordsToList
                $ P.tableOrders t
        , mkToOrder (ConT ''Order `AppT` ConT (mkName name))
                $ map (\(x, y) -> (name ++ upperFirst x ++ y, y))
                $ concatMap ordsToList
                $ P.tableOrders t
        , mkHalfDefined (ConT $ mkName name) name $ length $ tableColumns t
        ]

initialize :: (ToPersistables v, MonadCatchIO m)
           => Table -> v -> PostgresqlReader m ()
initialize t v = do
    conn <- ask
    tables <- liftIO $ H.getTables conn
    unless (map toLower (tableName t) `elem` tables) $ do
        let cols = zip (tableColumns t) $ toPersistables v
        let sql = "CREATE TABLE " ++ tableName t ++
                  "(id SERIAL" ++
                  concatMap go' cols ++ ")"
        withStmt sql (liftIO . flip execute [])
        mapM_ go $ tableUniques' t
        return ()
  where
    go' ((colName, (_, nullable)), p) = concat -- FIXME remove nullable
        [ ","
        , colName
        , " "
        , showSqlType $ sqlType p
        , if nullable then " NULL" else " NOT NULL" -- FIXME isNullable
        ]
    go (index, fields) = do
        let sql = "CREATE UNIQUE INDEX " ++ index ++ " ON " ++
                  tableName t ++ "(" ++ intercalate "," fields ++ ")"
        withStmt sql (liftIO . flip execute [])
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
    let cons = ConE $ mkName $ P.tableName t
    xs <- mapM (const $ newName "x") $ P.tableColumns t
    fs <- [|fromPersistValue|]
    let xs' = map (AppE fs . VarE) xs
    let pat = ListP $ map VarP xs
    ap' <- [|apE|]
    just <- [|Right|]
    let cons' = just `AppE` cons
    return
        [ Clause [pat] (NormalB $ foldl (go ap') cons' xs') []
        , Clause [WildP] (NormalB nothing) []
        ]
  where
    go ap' x y = InfixE (Just x) ap' (Just y)

insert :: (MonadCatchIO m, ToPersistables val, Num (Key val))
       => Table -> val -> PostgresqlReader m (Key val)
insert t val = do
    let sql = "INSERT INTO " ++ tableName t ++
              "(" ++ intercalate "," (map fst $ tableColumns t) ++
              ") VALUES(" ++
              intercalate "," (map (const "?") (tableColumns t)) ++ ") " ++
              "RETURNING id"
    withStmt sql $ \ins -> liftIO $ do
        execute ins $ toPersistValues val
        Just [PersistInt64 id'] <- fetchRow ins
        return $ fromIntegral id'

replace :: (MonadCatchIO m, Integral (Key v), ToPersistables v)
        => Table -> Key v -> v -> PostgresqlReader m ()
replace t k val = do
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (go . fst) $ tableColumns t) ++
              " WHERE id=?"
    withStmt sql $ \ins -> do
        liftIO $ execute ins $
                    map toPersistValue (toPersistables val)
                    ++ [PersistInt64 (fromIntegral k)]
  where
    go = (++ "=?")

get :: (Integral (Key v), MonadCatchIO m, FromPersistValues v)
    => Table -> Key v -> PostgresqlReader m (Maybe v)
get t k = do
    let sql = "SELECT * FROM " ++ tableName t ++ " WHERE id=?"
    withStmt sql $ \stmt -> do
        liftIO $ execute stmt [PersistInt64 $ fromIntegral k]
        res <- liftIO $ fetchRow stmt
        case res of
            Nothing -> return Nothing
            Just (_:vals) ->
                case fromPersistValues vals of
                    Left e -> error $ "get " ++ show k ++ ": " ++ e
                    Right v -> return $ Just v
            Just [] -> error "Database.Persist.Postgresql: Empty list in get"

select :: FromPersistValues val => Num key => MonadCatchIO m
       => Persistable (Filter val)
       => ToFieldName (Filter val) => ToFilter (Filter val)
       => ToFieldName (Order val) => ToOrder (Order val)
       => Table
       -> [Filter val]
       -> [Order val]
       -> PostgresqlReader m [(key, val)]
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
        liftIO $ execute stmt $ map toPersistValue filts
        liftIO $ go stmt id
  where
    orderClause o = toFieldName' o ++ case toOrder o of
                                        Asc -> ""
                                        Desc -> " DESC"
    fromPersistValues' (PersistInt64 x:xs) = do
        case fromPersistValues xs of
            Left e -> Left e
            Right xs' -> Right (fromIntegral x, xs')
    fromPersistValues' _ = Left "error in fromPersistValues'"
    go stmt front = do
        res <- fetchRow stmt
        case res of
            Nothing -> return $ front []
            Just vals -> do
                case fromPersistValues' vals of
                    Left _ -> go stmt front
                    Right row -> go stmt $ front . (:) row

filterClause :: (ToFilter f, ToFieldName f) => f -> String
filterClause f = if isNull f then nullClause else mainClause
  where
    mainClause = toFieldName f ++ showSqlFilter (toFilter f) ++ "?"
    nullClause =
        case toFilter f of
          Eq -> '(' : mainClause ++ " OR " ++ toFieldName f ++ " IS NULL)"
          Ne -> '(' : mainClause ++ " OR " ++ toFieldName f ++ " IS NOT NULL)"
          _ -> mainClause
    showSqlFilter Eq = "="
    showSqlFilter Ne = "<>"
    showSqlFilter Gt = ">"
    showSqlFilter Lt = "<"
    showSqlFilter Ge = ">="
    showSqlFilter Le = "<="

delete :: (Integral (Key v), MonadCatchIO m)
       => Table -> Key v -> PostgresqlReader m ()
delete t k =
    withStmt ("DELETE FROM " ++ tableName t ++ " WHERE id=?") $ \del -> do
        liftIO $ execute del [PersistInt64 $ fromIntegral k]
        return ()

deleteWhere :: (MonadCatchIO m, Persistable (Filter v), ToFilter (Filter v),
                ToFieldName (Filter v))
            => Table -> [Filter v] -> PostgresqlReader m ()
deleteWhere t filts = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
        sql = "DELETE FROM " ++ tableName t ++ wher
    withStmt sql $ \del -> liftIO $ execute del $ map toPersistValue filts

deleteBy :: (MonadCatchIO m, ToPersistables (Unique v), ToFieldNames (Unique v))
         => Table -> Unique v -> PostgresqlReader m ()
deleteBy t uniq = do
    let sql = "DELETE FROM " ++ tableName t ++ " WHERE " ++
              intercalate " AND " (map (++ "=?") $ toFieldNames' uniq)
    withStmt sql $ \del -> liftIO $
        execute del $ map toPersistValue $ toPersistables uniq

update :: (Integral (Key v), MonadCatchIO m, Persistable (Update v),
           ToFieldName (Update v))
       => Table -> Key v -> [Update v] -> PostgresqlReader m ()
update _ _ [] = return ()
update t k upds = do
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (++ "=?") $ map toFieldName' upds) ++
              " WHERE id=?"
    withStmt sql $ \up -> liftIO $
        execute up $ map toPersistValue upds ++
                          [PersistInt64 $ fromIntegral k]

updateWhere :: (MonadCatchIO m, Persistable (Filter v), Persistable (Update v),
                ToFieldName (Update v), ToFilter (Filter v),
                ToFieldName (Filter v))
            => Table -> [Filter v] -> [Update v] -> PostgresqlReader m ()
updateWhere _ _ [] = return ()
updateWhere t filts upds = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (++ "=?") $ map toFieldName' upds) ++ wher
    let dat = map toPersistValue upds ++ map toPersistValue filts
    withStmt sql $ \up -> liftIO $ execute up dat

getBy :: (Num (Key v), FromPersistValues v, MonadCatchIO m,
          ToPersistables (Unique v), ToFieldNames (Unique v))
      => Table -> Unique v -> PostgresqlReader m (Maybe (Key v, v))
getBy t uniq = do
    let sql = "SELECT * FROM " ++ tableName t ++ " WHERE " ++ sqlClause
    withStmt sql $ \stmt -> do
        liftIO $ execute stmt $ toPersistValues uniq
        row <- liftIO $ fetchRow stmt
        case row of
            Nothing -> return Nothing
            Just (PersistInt64 k:vals) ->
                case fromPersistValues vals of
                    Left _ -> return Nothing
                    Right x -> return $ Just (fromIntegral k, x)
            Just _ -> error "Database.Persist.Postgresql: Bad list in getBy"
  where
    sqlClause = intercalate " AND " $ map (++ "=?") $ toFieldNames' uniq

withStmt :: MonadCatchIO m => String -> (Statement -> PostgresqlReader m a)
         -> PostgresqlReader m a
withStmt sql f = do
    conn <- ask
    stmt <- liftIO $ prepare conn sql
    f stmt

prepare :: Connection -> String -> IO Statement
prepare = H.prepare

type Statement = H.Statement

fetchRow :: Statement -> IO (Maybe [PersistValue])
fetchRow = (fmap $ fmap $ map pFromSql) . H.fetchRow

execute :: Statement -> [PersistValue] -> IO ()
execute stmt vals = do
    _ <- H.execute stmt $ map pToSql vals
    return ()

pToSql :: PersistValue -> H.SqlValue
pToSql (PersistString s) = H.SqlString s
pToSql (PersistByteString bs) = H.SqlByteString bs
pToSql (PersistInt64 i) = H.SqlInt64 i
pToSql (PersistDouble d) = H.SqlDouble d
pToSql (PersistBool b) = H.SqlBool b
pToSql (PersistDay d) = H.SqlLocalDate d
pToSql (PersistTimeOfDay t) = H.SqlLocalTimeOfDay t
pToSql (PersistUTCTime t) = H.SqlUTCTime t
pToSql PersistNull = H.SqlNull

pFromSql :: H.SqlValue -> PersistValue
pFromSql (H.SqlString s) = PersistString s
pFromSql (H.SqlByteString bs) = PersistByteString bs
pFromSql (H.SqlWord32 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlWord64 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlInt32 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlInt64 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlInteger i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlChar c) = PersistInt64 $ fromIntegral $ fromEnum c
pFromSql (H.SqlBool b) = PersistBool b
pFromSql (H.SqlDouble b) = PersistDouble b
pFromSql (H.SqlRational b) = PersistDouble $ fromRational b
pFromSql (H.SqlLocalDate d) = PersistDay d
pFromSql (H.SqlLocalTimeOfDay d) = PersistTimeOfDay d
pFromSql (H.SqlUTCTime d) = PersistUTCTime d
pFromSql H.SqlNull = PersistNull
pFromSql x = PersistString $ H.fromSql x -- FIXME

tableName :: Table -> String
tableName t = "tbl" ++ P.tableName t

toField :: String -> String
toField = (++) "fld"

tableColumns :: Table -> [P.Column]
tableColumns = map (first toField) . P.tableColumns

tableUniques' :: Table -> [(String, [String])]
tableUniques' = map (second $ map toField) . P.tableUniques

toFieldName' :: ToFieldName x => x -> String
toFieldName' = toField . toFieldName

toFieldNames' :: ToFieldNames x => x -> [String]
toFieldNames' = map toField . toFieldNames
