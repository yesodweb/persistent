{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Persist.Sql.Orphan.PersistQuery
    ( deleteWhereCount
    , updateWhereCount
    , decorateSQLWithLimitOffset
    ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask, withReaderT)
import Data.ByteString.Char8 (readInteger)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Int (Int64)
import Data.List (transpose, inits, find)
import Data.Maybe (isJust)
import Data.Monoid (Monoid (..), (<>))
import qualified Data.Text as T
import Data.Text (Text)

import Database.Persist hiding (updateField)
import Database.Persist.Sql.Util (
    entityColumnNames, parseEntityValues, isIdField, updatePersistValue
  , mkUpdateText, commaSeparated, dbIdColumns)
import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Orphan.PersistStore (withRawQuery)

-- orphaned instance for convenience of modularity
instance PersistQueryRead SqlBackend where
    count filts = do
        conn <- ask
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
        let sql = mconcat
                [ "SELECT COUNT(*) FROM "
                , connEscapeName conn $ entityDB t
                , wher
                ]
        withRawQuery sql (getFiltsValues conn filts) $ do
            mm <- CL.head
            case mm of
              Just [PersistInt64 i] -> return $ fromIntegral i
              Just [PersistDouble i] ->return $ fromIntegral (truncate i :: Int64) -- gb oracle
              Just [PersistByteString i] -> case readInteger i of -- gb mssql
                                              Just (ret,"") -> return $ fromIntegral ret
                                              xs -> error $ "invalid number i["++show i++"] xs[" ++ show xs ++ "]"
              Just xs -> error $ "count:invalid sql  return xs["++show xs++"] sql["++show sql++"]"
              Nothing -> error $ "count:invalid sql returned nothing sql["++show sql++"]"
      where
        t = entityDef $ dummyFromFilts filts

    selectSourceRes filts opts = do
        conn <- ask
        srcRes <- rawQueryRes (sql conn) (getFiltsValues conn filts)
        return $ fmap (.| CL.mapM parse) srcRes
      where
        (limit, offset, orders) = limitOffsetOrder opts

        parse vals = case parseEntityValues t vals of
                       Left s -> liftIO $ throwIO $ PersistMarshalError s
                       Right row -> return row
        t = entityDef $ dummyFromFilts filts
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        ord conn =
            case map (orderClause False conn) orders of
                [] -> ""
                ords -> " ORDER BY " <> T.intercalate "," ords
        cols = commaSeparated . entityColumnNames t
        sql conn = connLimitOffset conn (limit,offset) (not (null orders)) $ mconcat
            [ "SELECT "
            , cols conn
            , " FROM "
            , connEscapeName conn $ entityDB t
            , wher conn
            , ord conn
            ]

    selectKeysRes filts opts = do
        conn <- ask
        srcRes <- rawQueryRes (sql conn) (getFiltsValues conn filts)
        return $ fmap (.| CL.mapM parse) srcRes
      where
        t = entityDef $ dummyFromFilts filts
        cols conn = T.intercalate "," $ dbIdColumns conn t


        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        sql conn = connLimitOffset conn (limit,offset) (not (null orders)) $ mconcat
            [ "SELECT "
            , cols conn
            , " FROM "
            , connEscapeName conn $ entityDB t
            , wher conn
            , ord conn
            ]

        (limit, offset, orders) = limitOffsetOrder opts

        ord conn =
            case map (orderClause False conn) orders of
                [] -> ""
                ords -> " ORDER BY " <> T.intercalate "," ords

        parse xs = do
            keyvals <- case entityPrimary t of
                      Nothing ->
                        case xs of
                           [PersistInt64 x] -> return [PersistInt64 x]
                           [PersistDouble x] -> return [PersistInt64 (truncate x)] -- oracle returns Double
                           _ -> return xs
                      Just pdef ->
                           let pks = map fieldHaskell $ compositeFields pdef
                               keyvals = map snd $ filter (\(a, _) -> let ret=isJust (find (== a) pks) in ret) $ zip (map fieldHaskell $ entityFields t) xs
                           in return keyvals
            case keyFromValues keyvals of
                Right k -> return k
                Left err -> error $ "selectKeysImpl: keyFromValues failed" <> show err
instance PersistQueryRead SqlReadBackend where
    count filts = withReaderT persistBackend $ count filts
    selectSourceRes filts opts = withReaderT persistBackend $ selectSourceRes filts opts
    selectKeysRes filts opts = withReaderT persistBackend $ selectKeysRes filts opts
instance PersistQueryRead SqlWriteBackend where
    count filts = withReaderT persistBackend $ count filts
    selectSourceRes filts opts = withReaderT persistBackend $ selectSourceRes filts opts
    selectKeysRes filts opts = withReaderT persistBackend $ selectKeysRes filts opts

instance PersistQueryWrite SqlBackend where
    deleteWhere filts = do
        _ <- deleteWhereCount filts
        return ()
    updateWhere filts upds = do
        _ <- updateWhereCount filts upds
        return ()
instance PersistQueryWrite SqlWriteBackend where
    deleteWhere filts = withReaderT persistBackend $ deleteWhere filts
    updateWhere filts upds = withReaderT persistBackend $ updateWhere filts upds

-- | Same as 'deleteWhere', but returns the number of rows affected.
--
-- @since 1.1.5
deleteWhereCount :: (PersistEntity val, MonadIO m, PersistEntityBackend val ~ SqlBackend, BackendCompatible SqlBackend backend)
                 => [Filter val]
                 -> ReaderT backend m Int64
deleteWhereCount filts = withReaderT projectBackend $ do
    conn <- ask
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
-- @since 1.1.5
updateWhereCount :: (PersistEntity val, MonadIO m, SqlBackend ~ PersistEntityBackend val, BackendCompatible SqlBackend backend)
                 => [Filter val]
                 -> [Update val]
                 -> ReaderT backend m Int64
updateWhereCount _ [] = return 0
updateWhereCount filts upds = withReaderT projectBackend $ do
    conn <- ask
    let wher = if null filts
                then ""
                else filterClause False conn filts
    let sql = mconcat
            [ "UPDATE "
            , connEscapeName conn $ entityDB t
            , " SET "
            , T.intercalate "," $ map (mkUpdateText conn) upds
            , wher
            ]
    let dat = map updatePersistValue upds `Data.Monoid.mappend`
              getFiltsValues conn filts
    rawExecuteCount sql dat
  where
    t = entityDef $ dummyFromFilts filts

fieldName ::  forall record typ. (PersistEntity record, PersistEntityBackend record ~ SqlBackend) => EntityField record typ -> DBName
fieldName f = fieldDB $ persistFieldDef f

dummyFromFilts :: [Filter v] -> Maybe v
dummyFromFilts _ = Nothing

getFiltsValues :: forall val. (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
               => SqlBackend -> [Filter val] -> [PersistValue]
getFiltsValues conn = snd . filterClauseHelper False False conn OrNullNo

data OrNull = OrNullYes | OrNullNo

filterClauseHelper :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
             => Bool -- ^ include table name?
             -> Bool -- ^ include WHERE?
             -> SqlBackend
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
        let t = entityDef $ dummyFromFilts [Filter field value pfilter]
        in case (isIdField field, entityPrimary t, allVals) of
                 (True, Just pdef, PersistList ys:_) ->
                    if length (compositeFields pdef) /= length ys
                       then error $ "wrong number of entries in compositeFields vs PersistList allVals=" ++ show allVals
                    else
                      case (allVals, pfilter, isCompFilter pfilter) of
                        ([PersistList xs], Eq, _) ->
                           let sqlcl=T.intercalate " and " (map (\a -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "? ")  (compositeFields pdef))
                           in (wrapSql sqlcl,xs)
                        ([PersistList xs], Ne, _) ->
                           let sqlcl=T.intercalate " or " (map (\a -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "? ")  (compositeFields pdef))
                           in (wrapSql sqlcl,xs)
                        (_, In, _) ->
                           let xxs = transpose (map fromPersistList allVals)
                               sqls=map (\(a,xs) -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "(" <> T.intercalate "," (replicate (length xs) " ?") <> ") ") (zip (compositeFields pdef) xxs)
                           in (wrapSql (T.intercalate " and " (map wrapSql sqls)), concat xxs)
                        (_, NotIn, _) ->
                           let xxs = transpose (map fromPersistList allVals)
                               sqls=map (\(a,xs) -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "(" <> T.intercalate "," (replicate (length xs) " ?") <> ") ") (zip (compositeFields pdef) xxs)
                           in (wrapSql (T.intercalate " or " (map wrapSql sqls)), concat xxs)
                        ([PersistList xs], _, True) ->
                           let zs = tail (inits (compositeFields pdef))
                               sql1 = map (\b -> wrapSql (T.intercalate " and " (map (\(i,a) -> sql2 (i==length b) a) (zip [1..] b)))) zs
                               sql2 islast a = connEscapeName conn (fieldDB a) <> (if islast then showSqlFilter pfilter else showSqlFilter Eq) <> "? "
                               sqlcl = T.intercalate " or " sql1
                           in (wrapSql sqlcl, concat (tail (inits xs)))
                        (_, BackendSpecificFilter _, _) -> error "unhandled type BackendSpecificFilter for composite/non id primary keys"
                        _ -> error $ "unhandled type/filter for composite/non id primary keys pfilter=" ++ show pfilter ++ " persistList="++show allVals
                 (True, Just pdef, []) ->
                     error $ "empty list given as filter value filter=" ++ show pfilter ++ " persistList=" ++ show allVals ++ " pdef=" ++ show pdef
                 (True, Just pdef, _) ->
                     error $ "unhandled error for composite/non id primary keys filter=" ++ show pfilter ++ " persistList=" ++ show allVals ++ " pdef=" ++ show pdef

                 _ ->   case (isNull, pfilter, length notNullVals) of
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
                            (False, NotIn, 0) -> ("1=1", [])
                            (True, NotIn, 0) -> (name <> " IS NOT NULL", [])
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
        isCompFilter Lt = True
        isCompFilter Le = True
        isCompFilter Gt = True
        isCompFilter Ge = True
        isCompFilter _ =  False

        wrapSql sqlcl = "(" <> sqlcl <> ")"
        fromPersistList (PersistList xs) = xs
        fromPersistList other = error $ "expected PersistList but found " ++ show other

        filterValueToPersistValues :: forall a.  PersistField a => FilterValue a -> [PersistValue]
        filterValueToPersistValues = \case
            FilterValue a -> [toPersistValue a]
            FilterValues as -> toPersistValue <$> as
            UnsafeValue x -> [toPersistValue x]

        orNullSuffix =
            case orNull of
                OrNullYes -> mconcat [" OR ", name, " IS NULL"]
                OrNullNo -> ""

        isNull = PersistNull `elem` allVals
        notNullVals = filter (/= PersistNull) allVals
        allVals = filterValueToPersistValues value
        tn = connEscapeName conn $ entityDB
           $ entityDef $ dummyFromFilts [Filter field value pfilter]
        name =
            (if includeTable
                then ((tn <> ".") <>)
                else id)
            $ connEscapeName conn $ fieldName field
        qmarks = case value of
                    FilterValue{} -> "(?)"
                    UnsafeValue{} -> "(?)"
                    FilterValues xs ->
                        let parens a = "(" <> a <> ")"
                            commas = T.intercalate ","
                            toQs = fmap $ const "?"
                            nonNulls = filter (/= PersistNull) $ map toPersistValue xs
                         in parens . commas . toQs $ nonNulls
        showSqlFilter Eq = "="
        showSqlFilter Ne = "<>"
        showSqlFilter Gt = ">"
        showSqlFilter Lt = "<"
        showSqlFilter Ge = ">="
        showSqlFilter Le = "<="
        showSqlFilter In = " IN "
        showSqlFilter NotIn = " NOT IN "
        showSqlFilter (BackendSpecificFilter s) = s

filterClause :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
             => Bool -- ^ include table name?
             -> SqlBackend
             -> [Filter val]
             -> Text
filterClause b c = fst . filterClauseHelper b True c OrNullNo

orderClause :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
            => Bool -- ^ include the table name
            -> SqlBackend
            -> SelectOpt val
            -> Text
orderClause includeTable conn o =
    case o of
        Asc  x -> name x
        Desc x -> name x <> " DESC"
        _ -> error "orderClause: expected Asc or Desc, not limit or offset"
  where
    dummyFromOrder :: SelectOpt a -> Maybe a
    dummyFromOrder _ = Nothing

    tn = connEscapeName conn $ entityDB $ entityDef $ dummyFromOrder o

    name :: (PersistEntityBackend record ~ SqlBackend, PersistEntity record)
         => EntityField record typ -> Text
    name x =
        (if includeTable
            then ((tn <> ".") <>)
            else id)
        $ connEscapeName conn $ fieldName x

-- | Generates sql for limit and offset for postgres, sqlite and mysql.
decorateSQLWithLimitOffset::Text -> (Int,Int) -> Bool -> Text -> Text
decorateSQLWithLimitOffset nolimit (limit,offset) _ sql =
    let
        lim = case (limit, offset) of
                (0, 0) -> ""
                (0, _) -> T.cons ' ' nolimit
                (_, _) -> " LIMIT " <> T.pack (show limit)
        off = if offset == 0
                    then ""
                    else " OFFSET " <> T.pack (show offset)
    in mconcat
            [ sql
            , lim
            , off
            ]
