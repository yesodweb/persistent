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
    ( GenericSql (..)
    , RowPopper
    , initialize
    , insert
    , get
    , replace
    , select
    , deleteWhere
    , update
    , updateWhere
    , getBy
    , delete
    , deleteBy
    ) where

import Database.Persist.Base hiding (PersistBackend (..))
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
    orderClause o = getFieldName t (persistOrderToFieldName o)
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
    go = map (getFieldName t) . persistUniqueToFieldNames
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
    go = getFieldName t . persistUpdateToFieldName

updateWhere :: (PersistEntity v, Monad m)
            => GenericSql m -> [Filter v] -> [Update v] -> m ()
updateWhere _ _ [] = return ()
updateWhere gs filts upds = do
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
    t = entityDef $ dummyFromFilts filts
    go = getFieldName t . persistUpdateToFieldName

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
tableColumns = map (\a@(x, y, z) -> (toField a, y, z)) . entityColumns

tableColumn :: EntityDef -> String -> (String, String, [String])
tableColumn t s = go $ entityColumns t
  where
    go [] = error $ "Unknown table column: " ++ s
    go ((x, y, z):rest)
        | x == s = (x, y, z)
        | otherwise = go rest

tableUniques' :: EntityDef -> [(String, [String])]
tableUniques' t = map (second $ map $ getFieldName t) $ entityUniques t
