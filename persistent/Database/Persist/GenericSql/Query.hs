{-# LANGUAGE RankNTypes #-}
module Database.Persist.GenericSql.Query
    ( getFiltsValues
    , filterClause
    , filterClauseNoWhere
    , dummyFromFilts
    , orderClause
    , getFieldName
    ) where

import Database.Persist.GenericSql.Internal
import Database.Persist.Base
import Database.Persist.Query

import Data.List (intercalate)

#if MIN_VERSION_monad_control(0, 3, 0)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import qualified Control.Exception as E
#define MBCIO MonadBaseControl IO
#else
import Control.Monad.IO.Control (MonadControlIO)
import Control.Exception.Control (onException)

#define MBCIO MonadControlIO
#endif

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

getFieldName :: EntityDef -> String -> RawName
getFieldName t s = rawFieldName $ tableColumn t s

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

tableColumn :: EntityDef -> String -> ColumnDef
tableColumn t s | s == id_ = ColumnDef id_ "Int64" []
  where id_ = unRawName $ rawTableIdName t
tableColumn t s = go $ entityColumns t
  where
    go [] = error $ "Unknown table column: " ++ s
    go (ColumnDef x y z:rest)
        | x == s = ColumnDef x y z
        | otherwise = go rest

#if MIN_VERSION_monad_control(0, 3, 0)
bracket :: MonadBaseControl IO m
        => m a       -- ^ computation to run first (\"acquire resource\")
        -> (a -> m b) -- ^ computation to run last (\"release resource\")
        -> (a -> m c) -- ^ computation to run in-between
        -> m c
bracket before after thing = control $ \runInIO ->
                               E.bracket (runInIO before)
                                         (\st -> runInIO $ restoreM st >>= after)
                                         (\st -> runInIO $ restoreM st >>= thing)
#endif
