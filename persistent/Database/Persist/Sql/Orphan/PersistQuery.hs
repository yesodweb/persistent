{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TODO: delete this module and get it in with SqlBackend.Internal
module Database.Persist.Sql.Orphan.PersistQuery
    ( deleteWhereCount
    , updateWhereCount
    , filterClause
    , filterClauseHelper
    , filterClauseWithVals
    , orderClause
    , FilterTablePrefix (..)
    , decorateSQLWithLimitOffset
    ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.ByteString.Char8 (readInteger)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List (find, inits, transpose)
import Data.Maybe (isJust)
import Data.Monoid (Monoid(..))
import Data.Text (Text)
import qualified Data.Text as T

import Database.Persist hiding (updateField)
import Database.Persist.Sql.Orphan.PersistStore (withRawQuery)
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Types.Internal
       (SqlBackend(..), SqlReadBackend, SqlWriteBackend)
import Database.Persist.Sql.Util
       ( commaSeparated
       , dbIdColumns
       , isIdField
       , keyAndEntityColumnNames
       , mkUpdateText
       , parseEntityValues
       , updatePersistValue
       )

-- orphaned instance for convenience of modularity
instance PersistQueryRead SqlBackend where
    count filts = do
        conn <- ask
        let wher = if null filts
                    then ""
                    else filterClause Nothing conn filts
        let sql = mconcat
                [ "SELECT COUNT(*) FROM "
                , connEscapeTableName conn t
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

    exists filts = do
        conn <- ask
        let wher = if null filts
                    then ""
                    else filterClause Nothing conn filts
        let sql = mconcat
                [ "SELECT EXISTS(SELECT 1 FROM "
                , connEscapeTableName conn t
                , wher
                , ")"
                ]
        withRawQuery sql (getFiltsValues conn filts) $ do
            mm <- CL.head
            case mm of
              Just [PersistBool b]  -> return b -- Postgres
              Just [PersistInt64 i] -> return $ i > 0 -- MySQL, SQLite
              Just [PersistDouble i] -> return $ (truncate i :: Int64) > 0 -- gb oracle
              Just [PersistByteString i] -> case readInteger i of -- gb mssql
                                              Just (ret,"") -> return $ ret > 0
                                              xs -> error $ "invalid number i["++show i++"] xs[" ++ show xs ++ "]"
              Just xs -> error $ "PersistQuery.exists: Expected a boolean, int, double, or bytestring; got: " ++ show xs ++ " for query: " ++ show sql
              Nothing -> error $ "PersistQuery.exists: Expected a boolean, int, double, or bytestring; got: Nothing for query: " ++ show sql
      where
        t = entityDef $ dummyFromFilts filts

    selectSourceRes filts opts = do
        conn <- ask
        srcRes <- rawQueryRes (sql conn) (getFiltsValues conn filts)
        return $ fmap (.| CL.mapM parse) srcRes
      where
        (limit, offset, orders) = limitOffsetOrder opts

        parse vals =
            case parseEntityValues t vals of
                Left s ->
                    liftIO $ throwIO $
                        PersistMarshalError ("selectSourceRes: " <> s <> ", vals: " <> T.pack (show vals ))
                Right row ->
                    return row
        t = entityDef $ dummyFromFilts filts
        wher conn = if null filts
                    then ""
                    else filterClause Nothing conn filts
        ord conn = orderClause Nothing conn orders
        cols = commaSeparated . toList . keyAndEntityColumnNames t
        sql conn = connLimitOffset conn (limit,offset) $ mconcat
            [ "SELECT "
            , cols conn
            , " FROM "
            , connEscapeTableName conn t
            , wher conn
            , ord conn
            ]

    selectKeysRes filts opts = do
        conn <- ask
        srcRes <- rawQueryRes (sql conn) (getFiltsValues conn filts)
        return $ fmap (.| CL.mapM parse) srcRes
      where
        t = entityDef $ dummyFromFilts filts
        cols conn = T.intercalate "," $ toList $ dbIdColumns conn t


        wher conn = if null filts
                    then ""
                    else filterClause Nothing conn filts
        sql conn = connLimitOffset conn (limit,offset) $ mconcat
            [ "SELECT "
            , cols conn
            , " FROM "
            , connEscapeTableName conn t
            , wher conn
            , ord conn
            ]

        (limit, offset, orders) = limitOffsetOrder opts

        ord conn = orderClause Nothing conn orders

        parse xs = do
            keyvals <- case entityPrimary t of
                      Nothing ->
                        case xs of
                           [PersistInt64 x] -> return [PersistInt64 x]
                           [PersistDouble x] -> return [PersistInt64 (truncate x)] -- oracle returns Double
                           _ -> return xs
                      Just pdef ->
                           let pks = map fieldHaskell $ toList $ compositeFields pdef
                               keyvals = map snd $ filter (\(a, _) -> let ret=isJust (find (== a) pks) in ret) $ zip (map fieldHaskell $ getEntityFields t) xs
                           in return keyvals
            case keyFromValues keyvals of
                Right k -> return k
                Left err -> error $ "selectKeysImpl: keyFromValues failed" <> show err
instance PersistQueryRead SqlReadBackend where
    count filts = withBaseBackend $ count filts
    exists filts = withBaseBackend $ exists filts
    selectSourceRes filts opts = withBaseBackend $ selectSourceRes filts opts
    selectKeysRes filts opts = withBaseBackend $ selectKeysRes filts opts
instance PersistQueryRead SqlWriteBackend where
    count filts = withBaseBackend $ count filts
    exists filts = withBaseBackend $ exists filts
    selectSourceRes filts opts = withBaseBackend $ selectSourceRes filts opts
    selectKeysRes filts opts = withBaseBackend $ selectKeysRes filts opts

instance PersistQueryWrite SqlBackend where
    deleteWhere filts = do
        _ <- deleteWhereCount filts
        return ()
    updateWhere filts upds = do
        _ <- updateWhereCount filts upds
        return ()
instance PersistQueryWrite SqlWriteBackend where
    deleteWhere filts = withBaseBackend $ deleteWhere filts
    updateWhere filts upds = withBaseBackend $ updateWhere filts upds

-- | Same as 'deleteWhere', but returns the number of rows affected.
--
-- @since 1.1.5
deleteWhereCount :: (PersistEntity val, MonadIO m, PersistEntityBackend val ~ SqlBackend, BackendCompatible SqlBackend backend)
                 => [Filter val]
                 -> ReaderT backend m Int64
deleteWhereCount filts = withCompatibleBackend $ do
    conn <- ask
    let t = entityDef $ dummyFromFilts filts
    let wher = if null filts
                then ""
                else filterClause Nothing conn filts
        sql = mconcat
            [ "DELETE FROM "
            , connEscapeTableName conn t
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
updateWhereCount filts upds = withCompatibleBackend $ do
    conn <- ask
    let wher = if null filts
                then ""
                else filterClause Nothing conn filts
    let sql = mconcat
            [ "UPDATE "
            , connEscapeTableName conn t
            , " SET "
            , T.intercalate "," $ map (mkUpdateText conn) upds
            , wher
            ]
    let dat = map updatePersistValue upds `Data.Monoid.mappend`
              getFiltsValues conn filts
    rawExecuteCount sql dat
  where
    t = entityDef $ dummyFromFilts filts

fieldName ::  forall record typ. (PersistEntity record) => EntityField record typ -> FieldNameDB
fieldName f = fieldDB $ persistFieldDef f

dummyFromFilts :: [Filter v] -> Maybe v
dummyFromFilts _ = Nothing

getFiltsValues :: forall val. (PersistEntity val)
               => SqlBackend -> [Filter val] -> [PersistValue]
getFiltsValues conn = snd . filterClauseHelper Nothing False conn OrNullNo

data OrNull = OrNullYes | OrNullNo

-- | Used when determining how to prefix a column name in a @WHERE@ clause.
--
-- @since 2.12.1.0
data FilterTablePrefix
    = PrefixTableName
    -- ^ Prefix the column with the table name. This is useful if the column
    -- name might be ambiguous.
    --
    -- @since 2.12.1.0
    | PrefixExcluded
    -- ^ Prefix the column name with the @EXCLUDED@ keyword. This is used with
    -- the Postgresql backend when doing @ON CONFLICT DO UPDATE@ clauses - see
    -- the documentation on @upsertWhere@ and @upsertManyWhere@.
    --
    -- @since 2.12.1.0

prefixByTable
    :: Maybe FilterTablePrefix
    -> Text -- ^ Table name
    -> (Text -> Text) -- ^ Prefixing function
prefixByTable tablePrefix tableName =
    case tablePrefix of
        Just PrefixTableName -> ((tableName <> ".") <>)
        Just PrefixExcluded -> (("EXCLUDED.") <>)
        _ -> id

filterClauseHelper
    :: (PersistEntity val)
    => Maybe FilterTablePrefix -- ^ include table name or PostgresSQL EXCLUDED
    -> Bool -- ^ include WHERE
    -> SqlBackend
    -> OrNull
    -> [Filter val]
    -> (Text, [PersistValue])
filterClauseHelper tablePrefix includeWhere conn orNull filters =
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
        in
            case (isIdField field, entityPrimary t, allVals) of
                (True, Just pdef, PersistList ys:_) ->
                    let cfields = toList $ compositeFields pdef in
                    if length cfields /= length ys
                    then error $ "wrong number of entries in compositeFields vs PersistList allVals=" ++ show allVals
                    else
                        case (allVals, pfilter, isCompFilter pfilter) of
                            ([PersistList xs], Eq, _) ->
                                let
                                    sqlcl =
                                        T.intercalate " and "
                                        (map (\a -> connEscapeFieldName conn (fieldDB a) <> showSqlFilter pfilter <> "? ")  cfields)
                                in
                                    (wrapSql sqlcl, xs)
                            ([PersistList xs], Ne, _) ->
                                let
                                    sqlcl =
                                        T.intercalate " or " (map (\a -> connEscapeFieldName conn (fieldDB a) <> showSqlFilter pfilter <> "? ")  cfields)
                                in
                                    (wrapSql sqlcl, xs)
                            (_, In, _) ->
                               let xxs = transpose (map fromPersistList allVals)
                                   sqls=map (\(a,xs) -> connEscapeFieldName conn (fieldDB a) <> showSqlFilter pfilter <> "(" <> T.intercalate "," (replicate (length xs) " ?") <> ") ") (zip cfields xxs)
                               in (wrapSql (T.intercalate " and " (map wrapSql sqls)), concat xxs)
                            (_, NotIn, _) ->
                                let
                                    xxs = transpose (map fromPersistList allVals)
                                    sqls = map (\(a,xs) -> connEscapeFieldName conn (fieldDB a) <> showSqlFilter pfilter <> "(" <> T.intercalate "," (replicate (length xs) " ?") <> ") ") (zip cfields xxs)
                                in
                                    (wrapSql (T.intercalate " or " (map wrapSql sqls)), concat xxs)
                            ([PersistList xs], _, True) ->
                               let zs = tail (inits (toList $ compositeFields pdef))
                                   sql1 = map (\b -> wrapSql (T.intercalate " and " (map (\(i,a) -> sql2 (i==length b) a) (zip [1..] b)))) zs
                                   sql2 islast a = connEscapeFieldName conn (fieldDB a) <> (if islast then showSqlFilter pfilter else showSqlFilter Eq) <> "? "
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
                OrNullYes -> mconcat [" OR "
                                      , name
                                      , " IS NULL"]
                OrNullNo -> ""

        isNull = PersistNull `elem` allVals
        notNullVals = filter (/= PersistNull) allVals
        allVals = filterValueToPersistValues value
        tn = connEscapeTableName conn $ entityDef $ dummyFromFilts [Filter field value pfilter]
        name = prefixByTable tablePrefix tn $ connEscapeFieldName conn (fieldName field)
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

-- |  Render a @['Filter' record]@ into a 'Text' value suitable for inclusion
-- into a SQL query.
--
-- @since 2.12.1.0
filterClause :: (PersistEntity val)
             => Maybe FilterTablePrefix -- ^ include table name or EXCLUDED
             -> SqlBackend
             -> [Filter val]
             -> Text
filterClause b c = fst . filterClauseHelper b True c OrNullNo

-- |  Render a @['Filter' record]@ into a 'Text' value suitable for inclusion
-- into a SQL query, as well as the @['PersistValue']@ to properly fill in the
-- @?@ place holders.
--
-- @since 2.12.1.0
filterClauseWithVals :: (PersistEntity val)
             => Maybe FilterTablePrefix -- ^ include table name or EXCLUDED
             -> SqlBackend
             -> [Filter val]
             -> (Text, [PersistValue])
filterClauseWithVals b c  = filterClauseHelper b True c OrNullNo

-- | Render a @['SelectOpt' record]@ made up *only* of 'Asc' and 'Desc' constructors
-- into a 'Text' value suitable for inclusion into a SQL query.
--
-- @since 2.13.2.0
orderClause :: (PersistEntity val)
            => Maybe FilterTablePrefix -- ^ include table name or EXCLUDED
            -> SqlBackend
            -> [SelectOpt val]
            -> Text
orderClause includeTable conn orders =
    if null orders
        then ""
        else
            " ORDER BY " <> T.intercalate ","
                (map (\case
                    Asc  x -> name x
                    Desc x -> name x <> " DESC"
                    _ -> error "orderClause: expected Asc or Desc, not limit or offset")
                    orders)
  where
    dummyFromOrder :: [SelectOpt a] -> Maybe a
    dummyFromOrder _ = Nothing

    tn = connEscapeTableName conn (entityDef $ dummyFromOrder orders)

    name :: (PersistEntity record)
         => EntityField record typ -> Text
    name x =
        prefixByTable includeTable tn
        $ connEscapeFieldName conn (fieldName x)

-- | Generates sql for limit and offset for postgres, sqlite and mysql.
decorateSQLWithLimitOffset
    :: Text
    -> (Int,Int)
    -> Text
    -> Text
decorateSQLWithLimitOffset nolimit (limit,offset) sql =
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
