{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Database.Persist.GenericSql
    ( Int64
    , module Database.Persist.Helper
    , persist
    , deriveGenericSql
    , RowPopper
    , GenericSql (..)
    ) where

import Database.Persist (Persist, Table, Key, Order, Filter, Update,
                         Unique, SqlType (..), PersistValue (..),
                         Persistable (..))
import qualified Database.Persist as P
import Database.Persist.Helper
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Language.Haskell.TH.Syntax as TH
import Data.List (intercalate)
import Control.Monad (unless, liftM)
import Data.Int (Int64)
import Database.Persist.Quasi
import Control.Arrow (first, second)

data GenericSql m = GenericSql
    { gsWithStmt :: forall a.
                    String -> [PersistValue] -> (RowPopper m -> m a) -> m a
    , gsExecute :: String -> [PersistValue] -> m ()
    , gsInsert :: String -> [String] -> [PersistValue] -> m Int64
    , gsTableExists :: String -> m Bool
    , gsKeyType :: String
    }

type RowPopper m = m (Maybe [PersistValue])

deriveGenericSql :: Type -> Name -> Exp -> Table -> Q [Dec]
deriveGenericSql wrap super gs t = do
    let name = P.tableName t
    let dt = dataTypeDec t
    let monad = wrap `AppT` VarT (mkName "m")

    fsv <- mkFromPersistValues t
    let sq =
          InstanceD [] (ConT ''FromPersistValues `AppT` ConT (mkName name))
            [ FunD (mkName "fromPersistValues") fsv
            ]

    let keysyn = TySynD (mkName $ name ++ "Id") [] $
                    ConT ''Key `AppT` ConT (mkName name)

    t' <- TH.lift t
    let mkFun s e = FunD (mkName s) [Clause [] (NormalB $ e `AppE` t') []]

    let gs' = fmap (`AppE` gs)
    init' <- gs' [|initialize|]
    insert' <- gs' [|insert|]
    replace' <- gs' [|replace|]
    get' <- gs' [|get|]
    getBy' <- gs' [|getBy|]
    select' <- gs' [|select|]
    deleteWhere' <- gs' [|deleteWhere|]
    delete' <- gs' [|delete|]
    deleteBy' <- gs' [|deleteBy|]
    update' <- gs' [|update|]
    updateWhere' <- gs' [|updateWhere|]

    let inst =
          InstanceD
            [ClassP super [VarT $ mkName "m"]]
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
                $ map (\(x, y) -> (toField x, (name ++ upperFirst x ++ y, y)))
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

initialize :: (ToPersistables v, Monad m)
           => GenericSql m -> Table -> v -> m ()
initialize gs t v = do
    doesExist <- gsTableExists gs $ tableName t
    unless doesExist $ do
        let cols = zip (tableColumns t) $ toPersistables v
        let sql = "CREATE TABLE " ++ tableName t ++
                  "(id " ++ gsKeyType gs ++
                  concatMap go' cols ++ ")"
        gsExecute gs sql []
        --mapM_ go $ tableUniques' t
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
        gsExecute gs sql []
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

insert :: (Monad m, ToPersistables val, Num (Key val))
       => GenericSql m -> Table -> val -> m (Key val)
insert gs t = liftM fromIntegral
            . gsInsert gs (tableName t) (map fst $ tableColumns t)
            . toPersistValues

replace :: (Integral (Key v), ToPersistables v, Monad m)
        => GenericSql m -> Table -> Key v -> v -> m ()
replace gs t k val = do
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (go . fst) $ tableColumns t) ++
              " WHERE id=?"
    gsExecute gs sql $
                    map toPersistValue (toPersistables val)
                    ++ [PersistInt64 (fromIntegral k)]
  where
    go = (++ "=?")

get :: (Integral (Key v), FromPersistValues v, Monad m)
    => GenericSql m -> Table -> Key v -> m (Maybe v)
get gs t k = do
    let sql = "SELECT * FROM " ++ tableName t ++ " WHERE id=?"
    gsWithStmt gs sql [PersistInt64 $ fromIntegral k] $ \pop -> do
        res <- pop
        case res of
            Nothing -> return Nothing
            Just (_:vals) ->
                case fromPersistValues vals of
                    Left e -> error $ "get " ++ show k ++ ": " ++ e
                    Right v -> return $ Just v
            Just [] -> error "Database.Persist.GenericSql: Empty list in get"

select :: ( FromPersistValues val, Num key
          , Persistable (Filter val), ToFieldName (Filter val)
          , ToFilter (Filter val), ToFieldName (Order val)
          , ToOrder (Order val), Monad m
          )
       => GenericSql m
       -> Table
       -> [Filter val]
       -> [Order val]
       -> m [(key, val)]
select gs t filts ords = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
        ord = if null ords
                then ""
                else " ORDER BY " ++
                     intercalate "," (map orderClause ords)
    let sql = "SELECT * FROM " ++ tableName t ++ wher ++ ord
    gsWithStmt gs sql (map toPersistValue filts) $ flip go id
  where
    orderClause o = toFieldName' o ++ case toOrder o of
                                        Asc -> ""
                                        Desc -> " DESC"
    fromPersistValues' (PersistInt64 x:xs) = do
        case fromPersistValues xs of
            Left e -> Left e
            Right xs' -> Right (fromIntegral x, xs')
    fromPersistValues' _ = Left "error in fromPersistValues'"
    go pop front = do
        res <- pop
        case res of
            Nothing -> return $ front []
            Just vals -> do
                case fromPersistValues' vals of
                    Left _ -> go pop front -- FIXME error?
                    Right row -> go pop $ front . (:) row

filterClause :: (ToFilter f, ToFieldName f) => f -> String
filterClause f = if isNull f then nullClause else mainClause
  where
    mainClause = toFieldName' f ++ showSqlFilter (toFilter f) ++ "?"
    nullClause =
        case toFilter f of
          Eq -> '(' : mainClause ++ " OR " ++ toFieldName' f ++ " IS NULL)"
          Ne -> '(' : mainClause ++ " OR " ++ toFieldName' f ++ " IS NOT NULL)"
          _ -> mainClause
    showSqlFilter Eq = "="
    showSqlFilter Ne = "<>"
    showSqlFilter Gt = ">"
    showSqlFilter Lt = "<"
    showSqlFilter Ge = ">="
    showSqlFilter Le = "<="

delete :: (Integral (Key v), Monad m)
       => GenericSql m -> Table -> Key v -> m ()
delete gs t k =
    gsExecute gs sql [PersistInt64 $ fromIntegral k]
  where
    sql = "DELETE FROM " ++ tableName t ++ " WHERE id=?"

deleteWhere :: (Persistable (Filter v), ToFilter (Filter v),
                ToFieldName (Filter v), Monad m)
            => GenericSql m -> Table -> [Filter v] -> m ()
deleteWhere gs t filts = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
        sql = "DELETE FROM " ++ tableName t ++ wher
    gsExecute gs sql $ map toPersistValue filts

deleteBy :: (ToPersistables (Unique v), ToFieldNames (Unique v), Monad m)
         => GenericSql m -> Table -> Unique v -> m ()
deleteBy gs t uniq = do
    let sql = "DELETE FROM " ++ tableName t ++ " WHERE " ++
              intercalate " AND " (map (++ "=?") $ toFieldNames' uniq)
    gsExecute gs sql $ map toPersistValue $ toPersistables uniq

update :: ( Integral (Key v), Persistable (Update v), ToFieldName (Update v)
          , Monad m)
       => GenericSql m -> Table -> Key v -> [Update v] -> m ()
update _ _ _ [] = return ()
update gs t k upds = do
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (++ "=?") $ map toFieldName' upds) ++
              " WHERE id=?"
    gsExecute gs sql $
        map toPersistValue upds ++ [PersistInt64 $ fromIntegral k]

updateWhere :: (Persistable (Filter v), Persistable (Update v),
                ToFieldName (Update v), ToFilter (Filter v),
                ToFieldName (Filter v), Monad m)
            => GenericSql m
            -> Table -> [Filter v] -> [Update v] -> m ()
updateWhere _ _ _ [] = return ()
updateWhere gs t filts upds = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (++ "=?") $ map toFieldName' upds) ++ wher
    let dat = map toPersistValue upds ++ map toPersistValue filts
    gsWithStmt gs sql dat  $ const $ return ()

getBy :: (Num (Key v), FromPersistValues v, Monad m,
          ToPersistables (Unique v), ToFieldNames (Unique v))
      => GenericSql m
      -> Table -> Unique v -> m (Maybe (Key v, v))
getBy gs t uniq = do
    let sql = "SELECT * FROM " ++ tableName t ++ " WHERE " ++ sqlClause
    gsWithStmt gs sql (toPersistValues uniq) $ \pop -> do
        row <- pop
        case row of
            Nothing -> return Nothing
            Just (PersistInt64 k:vals) ->
                case fromPersistValues vals of
                    Left _ -> return Nothing
                    Right x -> return $ Just (fromIntegral k, x)
            Just _ -> error "Database.Persist.GenericSql: Bad list in getBy"
  where
    sqlClause = intercalate " AND " $ map (++ "=?") $ toFieldNames' uniq

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
