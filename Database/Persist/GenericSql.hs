{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | This is a helper module for creating SQL backends. Regular users do not
-- need to use this module.
module Database.Persist.GenericSql
{-
    ( Int64
    , module Database.Persist.Helper
    , persist
    , RowPopper
    , GenericSql (..)
    ) -} where

import Database.Persist (PersistEntity (..),
                         SqlType (..), PersistValue (..),
                         PersistField (..))
import Database.Persist.Helper
import Data.List (intercalate)
import Control.Monad (unless, liftM)
import Data.Int (Int64)
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

{- FIXME
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
        , mkHalfDefined (ConT $ mkName name) name $ length $ tableColumns t
        ]
-}

initialize :: (Monad m, PersistEntity v) => GenericSql m -> v -> m ()
initialize gs v = do
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
    t = entityDef v
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
insert :: (Monad m, PersistEntity val)
       => GenericSql m -> val -> m (Key val)
insert gs v = liftM toPersistKey
            . gsInsert gs (tableName t) (map fst3 $ tableColumns t)
            . map toPersistValue . toPersistFields
            $ v
  where
    fst3 (x, _, _) = x
    t = entityDef v

replace :: (PersistEntity v, Monad m)
        => GenericSql m -> Key v -> v -> m ()
replace gs k val = do
    let t = entityDef val
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (go . fst3) $ tableColumns t) ++
              " WHERE id=?"
    gsExecute gs sql $
                    map toPersistValue (toPersistFields val)
                    ++ [PersistInt64 $ fromPersistKey k]
  where
    go = (++ "=?")
    fst3 (x, _, _) = x

dummyFromKey :: Key v -> v
dummyFromKey _ = error "dummyFromKey"

get :: (PersistEntity v, Monad m)
    => GenericSql m -> Key v -> m (Maybe v)
get gs k = do
    let t = entityDef $ dummyFromKey k
    let sql = "SELECT * FROM " ++ tableName t ++ " WHERE id=?"
    gsWithStmt gs sql [PersistInt64 $ fromPersistKey k] $ \pop -> do
        res <- pop
        case res of
            Nothing -> return Nothing
            Just (_:vals) ->
                case fromPersistValues vals of
                    Left e -> error $ "get " ++ showPersistKey k ++ ": " ++ e
                    Right v -> return $ Just v
            Just [] -> error "Database.Persist.GenericSql: Empty list in get"

select :: (PersistEntity val, Monad m)
       => GenericSql m
       -> [Filter val]
       -> [Order val]
       -> m [(Key val, val)]
select gs filts ords = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
        ord = if null ords
                then ""
                else " ORDER BY " ++
                     intercalate "," (map orderClause ords)
    let sql = "SELECT * FROM " ++ tableName t ++ wher ++ ord
    gsWithStmt gs sql (map persistFilterToValue filts) $ flip go id
  where
    t = entityDef $ dummyFromFilts filts
    orderClause o = toField (persistOrderToFieldName o)
                    ++ case persistOrderToOrder o of
                                        Asc -> ""
                                        Desc -> " DESC"
    fromPersistValues' (PersistInt64 x:xs) = do
        case fromPersistValues xs of
            Left e -> Left e
            Right xs' -> Right (toPersistKey x, xs')
    fromPersistValues' _ = Left "error in fromPersistValues'"
    go pop front = do
        res <- pop
        case res of
            Nothing -> return $ front []
            Just vals -> do
                case fromPersistValues' vals of
                    Left _ -> go pop front -- FIXME error?
                    Right row -> go pop $ front . (:) row

filterClause :: PersistEntity val => Filter val -> String
filterClause f = if persistFilterIsNull f then nullClause else mainClause
  where
    name = toField $ persistFilterToFieldName f
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

delete :: (PersistEntity v, Monad m) => GenericSql m -> Key v -> m ()
delete gs k =
    gsExecute gs sql [PersistInt64 $ fromPersistKey k]
  where
    t = entityDef $ dummyFromKey k
    sql = "DELETE FROM " ++ tableName t ++ " WHERE id=?"

dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"

deleteWhere :: (PersistEntity v, Monad m)
            => GenericSql m -> [Filter v] -> m ()
deleteWhere gs filts = do
    let t = entityDef $ dummyFromFilts filts
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
        sql = "DELETE FROM " ++ tableName t ++ wher
    gsExecute gs sql $ map persistFilterToValue filts

deleteBy :: (PersistEntity v, Monad m) => GenericSql m -> Unique v -> m ()
deleteBy gs uniq =
    gsExecute gs sql $ persistUniqueToValues uniq
  where
    t = entityDef $ dummyFromUnique uniq
    go = map toField . persistUniqueToFieldNames
    sql = "DELETE FROM " ++ tableName t ++ " WHERE " ++
          intercalate " AND " (map (++ "=?") $ go uniq)

update :: (PersistEntity v, Monad m)
       => GenericSql m -> Key v -> [Update v] -> m ()
update _ _ [] = return ()
update gs k upds = do
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (++ "=?") $ map go upds) ++
              " WHERE id=?"
    gsExecute gs sql $
        map persistUpdateToValue upds ++ [PersistInt64 $ fromPersistKey k]
  where
    t = entityDef $ dummyFromKey k
    go = toField . persistUpdateToFieldName

updateWhere :: (PersistEntity v, Monad m)
            => GenericSql m -> [Filter v] -> [Update v] -> m ()
updateWhere _ _ [] = return ()
updateWhere gs filts upds = do
    let t = entityDef $ dummyFromFilts filts
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (++ "=?") $ map go upds) ++ wher
    let dat = map persistUpdateToValue upds
           ++ map persistFilterToValue filts
    gsWithStmt gs sql dat  $ const $ return ()
  where
    go = toField . persistUpdateToFieldName

getBy :: (PersistEntity v, Monad m)
      => GenericSql m -> Unique v -> m (Maybe (Key v, v))
getBy gs uniq = do
    let sql = "SELECT * FROM " ++ tableName t ++ " WHERE " ++ sqlClause
    gsWithStmt gs sql (persistUniqueToValues uniq) $ \pop -> do
        row <- pop
        case row of
            Nothing -> return Nothing
            Just (PersistInt64 k:vals) ->
                case fromPersistValues vals of
                    Left _ -> return Nothing
                    Right x -> return $ Just (toPersistKey k, x)
            Just _ -> error "Database.Persist.GenericSql: Bad list in getBy"
  where
    sqlClause = intercalate " AND " $ map (++ "=?") $ toFieldNames' uniq
    t = entityDef $ dummyFromUnique uniq
    toFieldNames' = map toField . persistUniqueToFieldNames

dummyFromUnique :: Unique v -> v
dummyFromUnique _ = error "dummyFromUnique"

tableName :: EntityDef -> String
tableName t = "tbl" ++ entityName t

toField :: String -> String
toField = (++) "fld"

tableColumns :: EntityDef -> [(String, String, [String])]
tableColumns = map (\(x, y, z) -> (toField x, y, z)) . entityColumns

tableUniques' :: EntityDef -> [(String, [String])]
tableUniques' = map (second $ map toField) . entityUniques
