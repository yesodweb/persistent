{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Persist.Query.GenericSql
  ( PersistQuery (..)
    , SqlPersist (..)
    , filterClauseNoWhere
    , dummyFromFilts
    , orderClause
    , getFiltsValues
  )
  where

import Database.Persist.Store
import Database.Persist.Query
import Database.Persist.GenericSql
import Database.Persist.GenericSql.Internal
import qualified Database.Persist.GenericSql.Raw as R

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.List (intercalate)
import Data.Text (Text, pack)

import Data.Enumerator hiding (length, filter, consume, map)

import Control.Monad.IO.Control (MonadControlIO)
import Control.Exception (toException)


instance MonadControlIO m => PersistQuery SqlPersist m where
    update _ [] = return ()
    update k upds = do
        conn <- SqlPersist ask
        let go'' n Assign = n ++ "=?"
            go'' n Add = n ++ '=' : n ++ "+?"
            go'' n Subtract = n ++ '=' : n ++ "-?"
            go'' n Multiply = n ++ '=' : n ++ "*?"
            go'' n Divide = n ++ '=' : n ++ "/?"
        let go' (x, pu) = go'' (escapeName conn x) pu
        let sql = pack $ concat
                [ "UPDATE "
                , escapeName conn $ rawTableName t
                , " SET "
                , intercalate "," $ map (go' . go) upds
                , " WHERE id=?"
                ]
        execute' sql $
            map updatePersistValue upds ++ [unKey k]
      where
        t = entityDef $ dummyFromKey k
        go x = ( getFieldName t $ updateFieldName x
               , updateUpdate x
               )

    count filts = do
        conn <- SqlPersist ask
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
        let sql = pack $ concat
                [ "SELECT COUNT(*) FROM "
                , escapeName conn $ rawTableName t
                , wher
                ]
        withStmt' sql (getFiltsValues conn filts) $ \pop -> do
            Just [PersistInt64 i] <- pop
            return $ fromIntegral i
      where
        t = entityDef $ dummyFromFilts filts

    selectEnum filts opts =
        Iteratee . start
      where
        (limit, offset, orders) = limitOffsetOrder opts

        start x = do
            conn <- SqlPersist ask
            withStmt' (sql conn) (getFiltsValues conn filts) $ loop x
        loop (Continue k) pop = do
            res <- pop
            case res of
                Nothing -> return $ Continue k
                Just vals -> do
                    case fromPersistValues' vals of
                        Left s -> return $ Error $ toException
                                $ PersistMarshalError s
                        Right row -> do
                            step <- runIteratee $ k $ Chunks [row]
                            loop step pop
        loop step _ = return step
        t = entityDef $ dummyFromFilts filts
        fromPersistValues' (PersistInt64 x:xs) = do
            case fromPersistValues xs of
                Left e -> Left e
                Right xs' -> Right (Key $ PersistInt64 x, xs')
        fromPersistValues' _ = Left "error in fromPersistValues'"
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        ord conn =
            case map (orderClause False conn) orders of
                [] -> ""
                ords -> " ORDER BY " ++ intercalate "," ords
        lim conn = case (limit, offset) of
                (0, 0) -> ""
                (0, _) -> ' ' : noLimit conn
                (_, _) -> " LIMIT " ++ show limit
        off = if offset == 0
                    then ""
                    else " OFFSET " ++ show offset
        cols conn = intercalate "," $ (unRawName $ rawTableIdName t)
                   : (map (\(x, _, _) -> escapeName conn x) $ tableColumns t)
        sql conn = pack $ concat
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
            withStmt' (sql conn) (getFiltsValues conn filts) $ loop x
        loop (Continue k) pop = do
            res <- pop
            case res of
                Nothing -> return $ Continue k
                Just [PersistInt64 i] -> do
                    step <- runIteratee $ k $ Chunks [Key $ PersistInt64 i]
                    loop step pop
                Just y -> return $ Error $ toException $ PersistMarshalError
                        $ "Unexpected in selectKeys: " ++ show y
        loop step _ = return step
        t = entityDef $ dummyFromFilts filts
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        sql conn = pack $ concat
            [ "SELECT id FROM "
            , escapeName conn $ rawTableName t
            , wher conn
            ]

    deleteWhere filts = do
        conn <- SqlPersist ask
        let t = entityDef $ dummyFromFilts filts
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
            sql = pack $ concat
                [ "DELETE FROM "
                , escapeName conn $ rawTableName t
                , wher
                ]
        execute' sql $ getFiltsValues conn filts

    updateWhere _ [] = return ()
    updateWhere filts upds = do
        conn <- SqlPersist ask
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
        let sql = pack $ concat
                [ "UPDATE "
                , escapeName conn $ rawTableName t
                , " SET "
                , intercalate "," $ map (go' conn . go) upds
                , wher
                ]
        let dat = map updatePersistValue upds ++ getFiltsValues conn filts
        execute' sql dat
      where
        t = entityDef $ dummyFromFilts filts
        go'' n Assign = n ++ "=?"
        go'' n Add = n ++ '=' : n ++ "+?"
        go'' n Subtract = n ++ '=' : n ++ "-?"
        go'' n Multiply = n ++ '=' : n ++ "*?"
        go'' n Divide = n ++ '=' : n ++ "/?"
        go' conn (x, pu) = go'' (escapeName conn x) pu
        go x = ( getFieldName t $ updateFieldName x
               , updateUpdate x
               )

updatePersistValue :: Update v -> PersistValue
updatePersistValue (Update _ v _) = toPersistValue v

dummyFromKey :: Key SqlPersist v -> v 
dummyFromKey _ = error "dummyFromKey"

execute' :: MonadIO m => Text -> [PersistValue] -> SqlPersist m ()
execute' = R.execute

withStmt' :: MonadControlIO m => Text -> [PersistValue]
         -> (RowPopper (SqlPersist m) -> SqlPersist m a) -> SqlPersist m a
withStmt' = R.withStmt

getFiltsValues :: forall val.  PersistEntity val => Connection -> [Filter val] -> [PersistValue]
getFiltsValues conn = snd . filterClauseHelper False False conn

filterClause :: PersistEntity val
             => Bool -- ^ include table name?
             -> Connection -> [Filter val] -> String
filterClause b c = fst . filterClauseHelper b True c

filterClauseNoWhere :: PersistEntity val
                    => Bool -- ^ include table name?
                    -> Connection -> [Filter val] -> String
filterClauseNoWhere b c = fst . filterClauseHelper b False c

filterClauseHelper :: PersistEntity val
             => Bool -- ^ include table name?
             -> Bool -- ^ include WHERE?
             -> Connection -> [Filter val] -> (String, [PersistValue])
filterClauseHelper includeTable includeWhere conn filters =
    (if not (null sql) && includeWhere
        then " WHERE " ++ sql
        else sql, vals)
  where
    (sql, vals) = combineAND filters
    combineAND = combine " AND "

    combine s fs =
        (intercalate s $ map wrapP a, concat b)
      where
        (a, b) = unzip $ map go fs
        wrapP x = concat ["(", x, ")"]

    go (FilterAnd fs) = combineAND fs
    go (FilterOr fs)  = combine " OR " fs
    go (Filter field value pfilter) =
        case (isNull, pfilter, varCount) of
            (True, Eq, _) -> (name ++ " IS NULL", [])
            (True, Ne, _) -> (name ++ " IS NOT NULL", [])
            (False, Ne, _) -> (name ++ " IS NULL", [])
            -- We use 1=2 (and below 1=1) to avoid using TRUE and FALSE, since
            -- not all databases support those words directly.
            (_, In, 0) -> ("1=2", [])
            (False, In, _) -> (name ++ " IN " ++ qmarks, allVals)
            (True, In, _) -> (concat
                [ "("
                , name
                , " IS NULL OR "
                , name
                , " IN "
                , qmarks
                , ")"
                ], notNullVals)
            (_, NotIn, 0) -> ("1=1", [])
            (False, NotIn, _) -> (concat
                [ "("
                , name
                , " IS NULL OR "
                , name
                , " NOT IN "
                , qmarks
                , ")"
                ], notNullVals)
            (True, NotIn, _) -> (concat
                [ "("
                , name
                , " IS NOT NULL AND "
                , name
                , " NOT IN "
                , qmarks
                , ")"
                ], notNullVals)
            _ -> (name ++ showSqlFilter pfilter ++ "?", allVals)
      where
        filterValueToPersistValues :: forall a.  PersistField a => Either a [a] -> [PersistValue]
        filterValueToPersistValues v = map toPersistValue $ either return id v

        isNull = any (== PersistNull) allVals
        notNullVals = filter (/= PersistNull) allVals
        allVals = filterValueToPersistValues value
        t = entityDef $ dummyFromFilts [Filter field value pfilter]
        name =
            (if includeTable
                then (++) (escapeName conn (rawTableName t) ++ ".")
                else id)
            $ escapeName conn $ getFieldName t $ columnName $ persistColumnDef field
        qmarks = case value of
                    Left _ -> "?"
                    Right x ->
                        let x' = filter (/= PersistNull) $ map toPersistValue x
                         in '(' : intercalate "," (map (const "?") x') ++ ")"
        varCount = case value of
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
        showSqlFilter (BackendSpecificFilter s) = s

dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"

dummyFromOrder :: SelectOpt a -> a
dummyFromOrder _ = undefined

orderClause :: PersistEntity val => Bool -> Connection -> SelectOpt val -> String
orderClause includeTable conn o =
    case o of
        Asc  x -> name x
        Desc x -> name x ++ " DESC"
        _ -> error $ "expected Asc or Desc, not limit or offset"
  where
    cd x = persistColumnDef x
    t = entityDef $ dummyFromOrder o
    name x =
        (if includeTable
            then (++) (escapeName conn (rawTableName t) ++ ".")
            else id)
        $ escapeName conn $ getFieldName t $ columnName $ cd x
