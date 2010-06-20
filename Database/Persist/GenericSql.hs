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

import Database.Persist (PersistEntity, Key, Order, Filter, Update,
                         Unique, SqlType (..), PersistValue (..),
                         PersistField (..))
import Database.Persist.Helper
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Language.Haskell.TH.Syntax as TH
import Data.List (intercalate)
import Control.Monad (unless, liftM)
import Data.Int (Int64)
import Database.Persist.Quasi
import Control.Arrow (second)

data GenericSql m = GenericSql
    { gsWithStmt :: forall a.
                    String -> [PersistValue] -> (RowPopper m -> m a) -> m a
    , gsExecute :: String -> [PersistValue] -> m ()
    , gsInsert :: String -> [String] -> [PersistValue] -> m Int64
    , gsEntityDefExists :: String -> m Bool
    , gsKeyType :: String
    }

type RowPopper m = m (Maybe [PersistValue])

deriveGenericSql :: Type -> Exp -> EntityDef -> Q [Dec]
deriveGenericSql monad gs t = do
    let name = entityName t
    let dt = dataTypeDec t

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
            []
            (ConT ''PersistEntity `AppT` ConT (mkName name))
            [ persistMonadTypeDec monad t
            , keyTypeDec (name ++ "Id") "Int64" t
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

    tops <- mkToPersistFields (ConT $ mkName name)
                [(name, length $ tableColumns t)]
    topsUn <- mkToPersistFields (ConT ''Unique `AppT` ConT (mkName name))
            $ map (\(x, y) -> (x, length y))
            $ entityUniques t

    return
        [ dt, sq, inst, keysyn, tops, topsUn
        , mkToFieldName (ConT ''Update `AppT` ConT (mkName name))
                $ map (\(s, _, _) -> (name ++ upperFirst s, s))
                $ entityUpdates t
        , mkPersistField (ConT ''Update `AppT` ConT (mkName name))
                $ map (\(s, _, _) -> name ++ upperFirst s) $ entityUpdates t
        , mkToFieldNames (ConT ''Unique `AppT` ConT (mkName name))
                $ entityUniques t
        , mkPersistField (ConT ''Filter `AppT` ConT (mkName name))
                $ map (\(x, _, _, y) -> name ++ upperFirst x ++ show y)
                $ entityFilters t
        , mkToFieldName (ConT ''Filter `AppT` ConT (mkName name))
                $ map (\(x, _, _, y) -> (name ++ upperFirst x ++ show y, x))
                $ entityFilters t
        , mkToFilter (ConT ''Filter `AppT` ConT (mkName name))
                $ map (\(x, _, z, y) ->
                    (name ++ upperFirst x ++ show y, y, z))
                $ entityFilters t
        , mkToFieldName (ConT ''Order `AppT` ConT (mkName name))
                $ map (\(x, y) -> (name ++ upperFirst x ++ y, x))
                $ entityOrders t
        , mkToOrder (ConT ''Order `AppT` ConT (mkName name))
                $ map (\(x, y) -> (name ++ upperFirst x ++ y, y))
                $ entityOrders t
        , mkHalfDefined (ConT $ mkName name) name $ length $ tableColumns t
        ]

initialize :: (ToPersistFields v, Monad m, HalfDefined v)
           => GenericSql m -> EntityDef -> v -> m ()
initialize gs t v = do
    doesExist <- gsEntityDefExists gs $ tableName t
    unless doesExist $ do
        let cols = zip (tableColumns t) $ toPersistFields
                 $ halfDefined `asTypeOf` v
        let sql = "CREATE TABLE " ++ tableName t ++
                  "(id " ++ gsKeyType gs ++
                  concatMap go' cols ++ ")"
        gsExecute gs sql []
        mapM_ go $ tableUniques' t
  where
    go' ((colName, _, as), p) = concat
        [ ","
        , colName
        , " "
        , showSqlType $ sqlType p
        , if "null" `elem` as then " NULL" else " NOT NULL"
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

mkFromPersistValues :: EntityDef -> Q [Clause]
mkFromPersistValues t = do
    nothing <- [|Left "Invalid fromPersistValues input"|]
    let cons = ConE $ mkName $ entityName t
    xs <- mapM (const $ newName "x") $ entityColumns t
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

insert :: (Monad m, ToPersistFields val, Num (Key val))
       => GenericSql m -> EntityDef -> val -> m (Key val)
insert gs t = liftM fromIntegral
            . gsInsert gs (tableName t) (map fst3 $ tableColumns t)
            . toPersistValues
  where
    fst3 (x, _, _) = x

replace :: (Integral (Key v), ToPersistFields v, Monad m)
        => GenericSql m -> EntityDef -> Key v -> v -> m ()
replace gs t k val = do
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (go . fst3) $ tableColumns t) ++
              " WHERE id=?"
    gsExecute gs sql $
                    map toPersistValue (toPersistFields val)
                    ++ [PersistInt64 (fromIntegral k)]
  where
    go = (++ "=?")
    fst3 (x, _, _) = x

get :: (Integral (Key v), FromPersistValues v, Monad m)
    => GenericSql m -> EntityDef -> Key v -> m (Maybe v)
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
          , PersistField (Filter val), ToFieldName (Filter val)
          , ToFilter (Filter val), ToFieldName (Order val)
          , ToOrder (Order val), Monad m
          )
       => GenericSql m
       -> EntityDef
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
       => GenericSql m -> EntityDef -> Key v -> m ()
delete gs t k =
    gsExecute gs sql [PersistInt64 $ fromIntegral k]
  where
    sql = "DELETE FROM " ++ tableName t ++ " WHERE id=?"

deleteWhere :: (PersistField (Filter v), ToFilter (Filter v),
                ToFieldName (Filter v), Monad m)
            => GenericSql m -> EntityDef -> [Filter v] -> m ()
deleteWhere gs t filts = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
        sql = "DELETE FROM " ++ tableName t ++ wher
    gsExecute gs sql $ map toPersistValue filts

deleteBy :: (ToPersistFields (Unique v), ToFieldNames (Unique v), Monad m)
         => GenericSql m -> EntityDef -> Unique v -> m ()
deleteBy gs t uniq = do
    let sql = "DELETE FROM " ++ tableName t ++ " WHERE " ++
              intercalate " AND " (map (++ "=?") $ toFieldNames' uniq)
    gsExecute gs sql $ map toPersistValue $ toPersistFields uniq

update :: ( Integral (Key v), PersistField (Update v), ToFieldName (Update v)
          , Monad m)
       => GenericSql m -> EntityDef -> Key v -> [Update v] -> m ()
update _ _ _ [] = return ()
update gs t k upds = do
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (++ "=?") $ map toFieldName' upds) ++
              " WHERE id=?"
    gsExecute gs sql $
        map toPersistValue upds ++ [PersistInt64 $ fromIntegral k]

updateWhere :: (PersistField (Filter v), PersistField (Update v),
                ToFieldName (Update v), ToFilter (Filter v),
                ToFieldName (Filter v), Monad m)
            => GenericSql m
            -> EntityDef -> [Filter v] -> [Update v] -> m ()
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
          ToPersistFields (Unique v), ToFieldNames (Unique v))
      => GenericSql m
      -> EntityDef -> Unique v -> m (Maybe (Key v, v))
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

tableName :: EntityDef -> String
tableName t = "tbl" ++ entityName t

toField :: String -> String
toField = (++) "fld"

tableColumns :: EntityDef -> [(String, String, [String])]
tableColumns = map (\(x, y, z) -> (toField x, y, z)) . entityColumns

tableUniques' :: EntityDef -> [(String, [String])]
tableUniques' = map (second $ map toField) . entityUniques

toFieldName' :: ToFieldName x => x -> String
toFieldName' = toField . toFieldName

toFieldNames' :: ToFieldNames x => x -> [String]
toFieldNames' = map toField . toFieldNames
