{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME

module Database.Persist.Query.GenericSql
  ( PersistQuery (..)
    , SqlPersist (..)
    , filterClauseNoWhere
    , getFiltsValues
    , selectSourceConn
    , dummyFromFilts
    , orderClause
  )
  where

import qualified Prelude
import Prelude hiding ((++), unlines, concat, show)
import Data.Text (Text, pack, concat)
import Database.Persist.Store
import Database.Persist.Query.Internal
import Database.Persist.GenericSql
import Database.Persist.GenericSql.Internal
import qualified Database.Persist.GenericSql.Raw as R

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import Control.Exception (throwIO)
import qualified Data.Text as T
import Database.Persist.EntityDef
import Data.Monoid (Monoid, mappend, mconcat)

-- orphaned instance for convenience of modularity
instance C.ResourceIO m => PersistQuery SqlPersist m where
    update _ [] = return ()
    update k upds = do
        conn <- SqlPersist ask
        let go'' n Assign = n ++ "=?"
            go'' n Add = concat [n, "=", n, "+?"]
            go'' n Subtract = concat [n, "=", n, "-?"]
            go'' n Multiply = concat [n, "=", n, "*?"]
            go'' n Divide = concat [n, "=", n, "/?"]
        let go' (x, pu) = go'' (escapeName conn x) pu
        let sql = concat
                [ "UPDATE "
                , escapeName conn $ entityDB t
                , " SET "
                , T.intercalate "," $ map (go' . go) upds
                , " WHERE id=?"
                ]
        execute' sql $
            map updatePersistValue upds `mappend` [unKey k]
      where
        t = entityDef $ dummyFromKey k
        go x = (fieldDB $ updateFieldDef x, updateUpdate x)

    count filts = do
        conn <- SqlPersist ask
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
        let sql = concat
                [ "SELECT COUNT(*) FROM "
                , escapeName conn $ entityDB t
                , wher
                ]
        C.runResourceT $ R.withStmt sql (getFiltsValues conn filts) C.$$ do
            Just [PersistInt64 i] <- CL.head
            return $ fromIntegral i
      where
        t = entityDef $ dummyFromFilts filts

    selectSource filts opts = C.Source
        { C.sourcePull = do
            conn <- lift $ SqlPersist ask
            let src = R.withStmt (sql conn) (getFiltsValues conn filts) C.$= CL.mapM parse
            C.sourcePull src
        , C.sourceClose = return ()
        }
      where
        (limit, offset, orders) = limitOffsetOrder opts

        parse vals =
            case fromPersistValues' vals of
                Left s -> liftIO $ throwIO $ PersistMarshalError s
                Right row -> return row

        t = entityDef $ dummyFromFilts filts
        fromPersistValues' (PersistInt64 x:xs) = do
            case fromPersistValues xs of
                Left e -> Left e
                Right xs' -> Right (Entity (Key $ PersistInt64 x) xs')
        fromPersistValues' _ = Left "error in fromPersistValues'"
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        ord conn =
            case map (orderClause False conn) orders of
                [] -> ""
                ords -> " ORDER BY " ++ T.intercalate "," ords
        lim conn = case (limit, offset) of
                (0, 0) -> ""
                (0, _) -> T.cons ' ' $ noLimit conn
                (_, _) -> " LIMIT " ++ show limit
        off = if offset == 0
                    then ""
                    else " OFFSET " ++ show offset
        cols conn = T.intercalate ","
                  $ (escapeName conn $ entityID t)
                  : map (escapeName conn . fieldDB) (entityFields t)
        sql conn = concat
            [ "SELECT "
            , cols conn
            , " FROM "
            , escapeName conn $ entityDB t
            , wher conn
            , ord conn
            , lim conn
            , off
            ]

    selectKeys filts = C.Source
        { C.sourcePull = do
            conn <- lift $ SqlPersist ask
            let src = R.withStmt (sql conn) (getFiltsValues conn filts) C.$= CL.mapM parse
            C.sourcePull src
        , C.sourceClose = return ()
        }
      where
        parse [PersistInt64 i] = return $ Key $ PersistInt64 i
        parse y = liftIO $ throwIO $ PersistMarshalError $ "Unexpected in selectKeys: " ++ show y
        t = entityDef $ dummyFromFilts filts
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        sql conn = concat
            [ "SELECT "
            , escapeName conn $ entityID t
            , " FROM "
            , escapeName conn $ entityDB t
            , wher conn
            ]

    deleteWhere filts = do
        conn <- SqlPersist ask
        let t = entityDef $ dummyFromFilts filts
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
            sql = concat
                [ "DELETE FROM "
                , escapeName conn $ entityDB t
                , wher
                ]
        execute' sql $ getFiltsValues conn filts

    updateWhere _ [] = return ()
    updateWhere filts upds = do
        conn <- SqlPersist ask
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
        let sql = concat
                [ "UPDATE "
                , escapeName conn $ entityDB t
                , " SET "
                , T.intercalate "," $ map (go' conn . go) upds
                , wher
                ]
        let dat = map updatePersistValue upds `mappend`
                  getFiltsValues conn filts
        execute' sql dat
      where
        t = entityDef $ dummyFromFilts filts
        go'' n Assign = n ++ "=?"
        go'' n Add = concat [n, "=", n, "+?"]
        go'' n Subtract = concat [n, "=", n, "-?"]
        go'' n Multiply = concat [n, "=", n, "*?"]
        go'' n Divide = concat [n, "=", n, "/?"]
        go' conn (x, pu) = go'' (escapeName conn x) pu
        go x = (fieldDB $ updateFieldDef x, updateUpdate x)

updatePersistValue :: Update v -> PersistValue
updatePersistValue (Update _ v _) = toPersistValue v

dummyFromKey :: Key SqlPersist v -> v 
dummyFromKey _ = error "dummyFromKey"

execute' :: MonadIO m => Text -> [PersistValue] -> SqlPersist m ()
execute' = R.execute

getFiltsValues :: forall val.  PersistEntity val => Connection -> [Filter val] -> [PersistValue]
getFiltsValues conn = snd . filterClauseHelper False False conn

filterClause :: PersistEntity val
             => Bool -- ^ include table name?
             -> Connection
             -> [Filter val]
             -> Text
filterClause b c = fst . filterClauseHelper b True c

filterClauseNoWhere :: PersistEntity val
                    => Bool -- ^ include table name?
                    -> Connection
                    -> [Filter val]
                    -> Text
filterClauseNoWhere b c = fst . filterClauseHelper b False c

filterClauseHelper :: PersistEntity val
             => Bool -- ^ include table name?
             -> Bool -- ^ include WHERE?
             -> Connection
             -> [Filter val]
             -> (Text, [PersistValue])
filterClauseHelper includeTable includeWhere conn filters =
    (if not (T.null sql) && includeWhere
        then " WHERE " ++ sql
        else sql, vals)
  where
    (sql, vals) = combineAND filters
    combineAND = combine " AND "

    combine s fs =
        (T.intercalate s $ map wrapP a, mconcat b)
      where
        (a, b) = unzip $ map go fs
        wrapP x = T.concat ["(", x, ")"]

    go (FilterAnd []) = ("1=1", [])
    go (FilterAnd fs) = combineAND fs
    go (FilterOr []) = ("1=0", [])
    go (FilterOr fs)  = combine " OR " fs
    go (Filter field value pfilter) =
        case (isNull, pfilter, varCount) of
            (True, Eq, _) -> (name ++ " IS NULL", [])
            (True, Ne, _) -> (name ++ " IS NOT NULL", [])
            (False, Ne, _) -> (T.concat
                [ "("
                , name
                , " IS NULL OR "
                , name
                , " <> "
                , qmarks
                , ")"
                ], notNullVals)
            -- We use 1=2 (and below 1=1) to avoid using TRUE and FALSE, since
            -- not all databases support those words directly.
            (_, In, 0) -> ("1=2", [])
            (False, In, _) -> (name ++ " IN " ++ qmarks, allVals)
            (True, In, _) -> (T.concat
                [ "("
                , name
                , " IS NULL OR "
                , name
                , " IN "
                , qmarks
                , ")"
                ], notNullVals)
            (_, NotIn, 0) -> ("1=1", [])
            (False, NotIn, _) -> (T.concat
                [ "("
                , name
                , " IS NULL OR "
                , name
                , " NOT IN "
                , qmarks
                , ")"
                ], notNullVals)
            (True, NotIn, _) -> (T.concat
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
        tn = escapeName conn $ entityDB
           $ entityDef $ dummyFromFilts [Filter field value pfilter]
        name =
            (if includeTable
                then ((tn ++ ".") ++)
                else id)
            $ escapeName conn $ fieldDB $ persistFieldDef field
        qmarks = case value of
                    Left _ -> "?"
                    Right x ->
                        let x' = filter (/= PersistNull) $ map toPersistValue x
                         in "(" ++ T.intercalate "," (map (const "?") x') ++ ")"
        varCount = case value of
                    Left _ -> 1
                    Right x -> length x
        showSqlFilter Eq = "="
        showSqlFilter Ne = "++"
        showSqlFilter Gt = ">"
        showSqlFilter Lt = "<"
        showSqlFilter Ge = ">="
        showSqlFilter Le = "<="
        showSqlFilter In = " IN "
        showSqlFilter NotIn = " NOT IN "
        showSqlFilter (BackendSpecificFilter s) = s

infixr 5 ++
(++) :: Text -> Text -> Text
(++) = mappend

show :: Show a => a -> Text
show = pack . Prelude.show

-- | Equivalent to 'selectSource', but instead of getting the connection from
-- the environment inside a 'SqlPersist' monad, provide an explicit
-- 'Connection'. This can allow you to use the returned 'Source' in an
-- arbitrary monad.
selectSourceConn :: (C.ResourceIO m, PersistEntity val, SqlPersist ~ PersistEntityBackend val)
                 => Connection
                 -> [Filter val]
                 -> [SelectOpt val]
                 -> C.Source m (Entity val)
selectSourceConn conn fs opts =
    C.transSource (flip runSqlConn conn) (selectSource fs opts)

dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"

orderClause :: PersistEntity val
            => Bool -- ^ include the table name
            -> Connection
            -> SelectOpt val
            -> Text
orderClause includeTable conn o =
    case o of
        Asc  x -> name $ persistFieldDef x
        Desc x -> name (persistFieldDef x) ++ " DESC"
        _ -> error $ "orderClause: expected Asc or Desc, not limit or offset"
  where
    dummyFromOrder :: SelectOpt a -> a
    dummyFromOrder _ = undefined

    tn = escapeName conn $ entityDB $ entityDef $ dummyFromOrder o

    name x =
        (if includeTable
            then ((tn ++ ".") ++)
            else id)
        $ escapeName conn $ fieldDB x
