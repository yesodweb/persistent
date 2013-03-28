{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Sql.Orphan.PersistQuery
    ( deleteWhereCount
    , updateWhereCount
    ) where

import Database.Persist
import Database.Persist.Sql.Types
import Database.Persist.Sql.Class
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Orphan.PersistStore ()
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid (Monoid (..), (<>))
import Data.Int (Int64)
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Exception (throwIO)
import qualified Data.Conduit.List as CL
import Data.Conduit

-- orphaned instance for convenience of modularity
instance (MonadResource m, MonadLogger m) => PersistQuery (SqlPersistT m) where
    update _ [] = return ()
    update k upds = do
        conn <- askSqlConn
        let go'' n Assign = n <> "=?"
            go'' n Add = T.concat [n, "=", n, "+?"]
            go'' n Subtract = T.concat [n, "=", n, "-?"]
            go'' n Multiply = T.concat [n, "=", n, "*?"]
            go'' n Divide = T.concat [n, "=", n, "/?"]
        let go' (x, pu) = go'' (connEscapeName conn x) pu
        let sql = T.concat
                [ "UPDATE "
                , connEscapeName conn $ entityDB t
                , " SET "
                , T.intercalate "," $ map (go' . go) upds
                , " WHERE "
                , connEscapeName conn $ entityID t
                , "=?"
                ]
        rawExecute sql $
            map updatePersistValue upds `mappend` [unKey k]
      where
        t = entityDef $ dummyFromKey k
        go x = (fieldDB $ updateFieldDef x, updateUpdate x)

    count filts = do
        conn <- askSqlConn
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
        let sql = mconcat
                [ "SELECT COUNT(*) FROM "
                , connEscapeName conn $ entityDB t
                , wher
                ]
        rawQuery sql (getFiltsValues conn filts) $$ do
            Just [PersistInt64 i] <- CL.head
            return $ fromIntegral i
      where
        t = entityDef $ dummyFromFilts filts

    selectSource filts opts = do
        conn <- lift askSqlConn
        rawQuery (sql conn) (getFiltsValues conn filts) $= CL.mapM parse
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
                ords -> " ORDER BY " <> T.intercalate "," ords
        lim conn = case (limit, offset) of
                (0, 0) -> ""
                (0, _) -> T.cons ' ' $ connNoLimit conn
                (_, _) -> " LIMIT " <> T.pack (show limit)
        off = if offset == 0
                    then ""
                    else " OFFSET " <> T.pack (show offset)
        cols conn = T.intercalate ","
                  $ (connEscapeName conn $ entityID t)
                  : map (connEscapeName conn . fieldDB) (entityFields t)
        sql conn = mconcat
            [ "SELECT "
            , cols conn
            , " FROM "
            , connEscapeName conn $ entityDB t
            , wher conn
            , ord conn
            , lim conn
            , off
            ]

    selectKeys filts opts = do
        conn <- lift askSqlConn
        rawQuery (sql conn) (getFiltsValues conn filts) $= CL.mapM parse
      where
        parse [PersistInt64 i] = return $ Key $ PersistInt64 i
        parse y = liftIO $ throwIO $ PersistMarshalError $ "Unexpected in selectKeys: " <> T.pack (show y)
        t = entityDef $ dummyFromFilts filts
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        sql conn = mconcat
            [ "SELECT "
            , connEscapeName conn $ entityID t
            , " FROM "
            , connEscapeName conn $ entityDB t
            , wher conn
            , ord conn
            , lim conn
            , off
            ]

        (limit, offset, orders) = limitOffsetOrder opts

        ord conn =
            case map (orderClause False conn) orders of
                [] -> ""
                ords -> " ORDER BY " <> T.intercalate "," ords
        lim conn = case (limit, offset) of
                (0, 0) -> ""
                (0, _) -> T.cons ' ' $ connNoLimit conn
                (_, _) -> " LIMIT " <> T.pack (show limit)
        off = if offset == 0
                    then ""
                    else " OFFSET " <> T.pack (show offset)

    deleteWhere filts = do
        _ <- deleteWhereCount filts
        return ()

    updateWhere filts upds = do
        _ <- updateWhereCount filts upds
        return ()

-- | Same as 'deleteWhere', but returns the number of rows affected.
--
-- Since 1.1.5
deleteWhereCount :: (PersistEntity val, MonadSqlPersist m)
                 => [Filter val]
                 -> m Int64
deleteWhereCount filts = do
    conn <- askSqlConn
    let t = entityDef $ dummyFromFilts filts
    let wher = if null filts
                then ""
                else filterClause False conn filts
        sql = mconcat
            [ "DELETE FROM "
            , connEscapeName conn $ entityDB t
            , wher
            ]
    rawExecuteCount sql $ getFiltsValues conn filts

-- | Same as 'updateWhere', but returns the number of rows affected.
--
-- Since 1.1.5
updateWhereCount :: (PersistEntity val, MonadSqlPersist m)
                 => [Filter val]
                 -> [Update val]
                 -> m Int64
updateWhereCount _ [] = return 0
updateWhereCount filts upds = do
    conn <- askSqlConn
    let wher = if null filts
                then ""
                else filterClause False conn filts
    let sql = mconcat
            [ "UPDATE "
            , connEscapeName conn $ entityDB t
            , " SET "
            , T.intercalate "," $ map (go' conn . go) upds
            , wher
            ]
    let dat = map updatePersistValue upds `mappend`
              getFiltsValues conn filts
    rawExecuteCount sql dat
  where
    t = entityDef $ dummyFromFilts filts
    go'' n Assign = n <> "=?"
    go'' n Add = mconcat [n, "=", n, "+?"]
    go'' n Subtract = mconcat [n, "=", n, "-?"]
    go'' n Multiply = mconcat [n, "=", n, "*?"]
    go'' n Divide = mconcat [n, "=", n, "/?"]
    go' conn (x, pu) = go'' (connEscapeName conn x) pu
    go x = (fieldDB $ updateFieldDef x, updateUpdate x)

updateFieldDef :: PersistEntity v => Update v -> FieldDef
updateFieldDef (Update f _ _) = persistFieldDef f

dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"

getFiltsValues :: forall val.  PersistEntity val => Connection -> [Filter val] -> [PersistValue]
getFiltsValues conn = snd . filterClauseHelper False False conn OrNullNo

data OrNull = OrNullYes | OrNullNo

filterClauseHelper :: PersistEntity val
             => Bool -- ^ include table name?
             -> Bool -- ^ include WHERE?
             -> Connection
             -> OrNull
             -> [Filter val]
             -> (Text, [PersistValue])
filterClauseHelper includeTable includeWhere conn orNull filters =
    (if not (T.null sql) && includeWhere
        then " WHERE " <> sql
        else sql, vals)
  where
    (sql, vals) = combineAND filters
    combineAND = combine " AND "

    combine s fs =
        (T.intercalate s $ map wrapP a, mconcat b)
      where
        (a, b) = unzip $ map go fs
        wrapP x = T.concat ["(", x, ")"]

    go (BackendFilter _) = error "BackendFilter not expected"
    go (FilterAnd []) = ("1=1", [])
    go (FilterAnd fs) = combineAND fs
    go (FilterOr []) = ("1=0", [])
    go (FilterOr fs)  = combine " OR " fs
    go (Filter field value pfilter) =
        case (isNull, pfilter, varCount) of
            (True, Eq, _) -> (name <> " IS NULL", [])
            (True, Ne, _) -> (name <> " IS NOT NULL", [])
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
            (_, In, 0) -> ("1=2" <> orNullSuffix, [])
            (False, In, _) -> (name <> " IN " <> qmarks <> orNullSuffix, allVals)
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
            _ -> (name <> showSqlFilter pfilter <> "?" <> orNullSuffix, allVals)
      where
        filterValueToPersistValues :: forall a.  PersistField a => Either a [a] -> [PersistValue]
        filterValueToPersistValues v = map toPersistValue $ either return id v

        orNullSuffix =
            case orNull of
                OrNullYes -> mconcat [" OR ", name, " IS NULL"]
                OrNullNo -> ""

        isNull = any (== PersistNull) allVals
        notNullVals = filter (/= PersistNull) allVals
        allVals = filterValueToPersistValues value
        tn = connEscapeName conn $ entityDB
           $ entityDef $ dummyFromFilts [Filter field value pfilter]
        name =
            (if includeTable
                then ((tn <> ".") <>)
                else id)
            $ connEscapeName conn $ fieldDB $ persistFieldDef field
        qmarks = case value of
                    Left _ -> "?"
                    Right x ->
                        let x' = filter (/= PersistNull) $ map toPersistValue x
                         in "(" <> T.intercalate "," (map (const "?") x') <> ")"
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

updatePersistValue :: Update v -> PersistValue
updatePersistValue (Update _ v _) = toPersistValue v

filterClause :: PersistEntity val
             => Bool -- ^ include table name?
             -> Connection
             -> [Filter val]
             -> Text
filterClause b c = fst . filterClauseHelper b True c OrNullNo

orderClause :: PersistEntity val
            => Bool -- ^ include the table name
            -> Connection
            -> SelectOpt val
            -> Text
orderClause includeTable conn o =
    case o of
        Asc  x -> name $ persistFieldDef x
        Desc x -> name (persistFieldDef x) <> " DESC"
        _ -> error $ "orderClause: expected Asc or Desc, not limit or offset"
  where
    dummyFromOrder :: SelectOpt a -> a
    dummyFromOrder _ = undefined

    tn = connEscapeName conn $ entityDB $ entityDef $ dummyFromOrder o

    name x =
        (if includeTable
            then ((tn <> ".") <>)
            else id)
        $ connEscapeName conn $ fieldDB x

dummyFromKey :: KeyBackend SqlBackend v -> v
dummyFromKey _ = error "dummyFromKey"
