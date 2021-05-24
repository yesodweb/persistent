{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Persist.Sql.Orphan.PersistStore
    ( withRawQuery
    , BackendKey(..)
    , toSqlKey
    , fromSqlKey
    , getFieldName
    , getTableName
    , tableDBName
    , fieldDBName
    ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Acquire (with)
import qualified Data.Aeson as A
import Data.ByteString.Char8 (readInteger)
import Data.Conduit (ConduitM, runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as Foldable
import Data.Function (on)
import Data.Int (Int64)
import Data.List (find, nubBy)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Web.PathPieces (PathPiece)

import Database.Persist
import Database.Persist.Class ()
import Database.Persist.Sql.Class (PersistFieldSql)
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Types
import Database.Persist.Sql.Types.Internal
import Database.Persist.Sql.Util
       ( commaSeparated
       , dbIdColumns
       , keyAndEntityColumnNames
       , mkInsertValues
       , mkUpdateText
       , parseEntityValues
       , updatePersistValue
       )

withRawQuery :: MonadIO m
             => Text
             -> [PersistValue]
             -> ConduitM [PersistValue] Void IO a
             -> ReaderT SqlBackend m a
withRawQuery sql vals sink = do
    srcRes <- rawQueryRes sql vals
    liftIO $ with srcRes (\src -> runConduit $ src .| sink)

toSqlKey :: ToBackendKey SqlBackend record => Int64 -> Key record
toSqlKey = fromBackendKey . SqlBackendKey

fromSqlKey :: ToBackendKey SqlBackend record => Key record -> Int64
fromSqlKey = unSqlBackendKey . toBackendKey

whereStmtForKey :: PersistEntity record => SqlBackend -> Key record -> Text
whereStmtForKey conn k =
    T.intercalate " AND "
  $ Foldable.toList
  $ fmap (<> "=? ")
  $ dbIdColumns conn entDef
  where
    entDef = entityDef $ dummyFromKey k

whereStmtForKeys :: PersistEntity record => SqlBackend -> [Key record] -> Text
whereStmtForKeys conn ks = T.intercalate " OR " $ whereStmtForKey conn `fmap` ks

-- | get the SQL string for the table that a PeristEntity represents
-- Useful for raw SQL queries
--
-- Your backend may provide a more convenient tableName function
-- which does not operate in a Monad
getTableName :: forall record m backend.
             ( PersistEntity record
             , BackendCompatible SqlBackend backend
             , Monad m
             ) => record -> ReaderT backend m Text
getTableName rec = withCompatibleBackend $ do
    conn <- ask
    return $ connEscapeTableName conn (entityDef $ Just rec)

-- | useful for a backend to implement tableName by adding escaping
tableDBName :: (PersistEntity record) => record -> EntityNameDB
tableDBName rec = getEntityDBName $ entityDef (Just rec)

-- | get the SQL string for the field that an EntityField represents
-- Useful for raw SQL queries
--
-- Your backend may provide a more convenient fieldName function
-- which does not operate in a Monad
getFieldName :: forall record typ m backend.
             ( PersistEntity record
             , PersistEntityBackend record ~ SqlBackend
             , BackendCompatible SqlBackend backend
             , Monad m
             )
             => EntityField record typ -> ReaderT backend m Text
getFieldName rec = withCompatibleBackend $ do
    conn <- ask
    return $ connEscapeFieldName conn (fieldDB $ persistFieldDef rec)

-- | useful for a backend to implement fieldName by adding escaping
fieldDBName :: forall record typ. (PersistEntity record) => EntityField record typ -> FieldNameDB
fieldDBName = fieldDB . persistFieldDef


instance PersistCore SqlBackend where
    newtype BackendKey SqlBackend = SqlBackendKey { unSqlBackendKey :: Int64 }
        deriving stock (Show, Read, Eq, Ord, Generic)
        deriving newtype (Num, Integral, PersistField, PersistFieldSql, PathPiece, ToHttpApiData, FromHttpApiData, Real, Enum, Bounded, A.ToJSON, A.FromJSON)

instance PersistCore SqlReadBackend where
    newtype BackendKey SqlReadBackend = SqlReadBackendKey { unSqlReadBackendKey :: Int64 }
        deriving stock (Show, Read, Eq, Ord, Generic)
        deriving newtype (Num, Integral, PersistField, PersistFieldSql, PathPiece, ToHttpApiData, FromHttpApiData, Real, Enum, Bounded, A.ToJSON, A.FromJSON)

instance PersistCore SqlWriteBackend where
    newtype BackendKey SqlWriteBackend = SqlWriteBackendKey { unSqlWriteBackendKey :: Int64 }
        deriving stock (Show, Read, Eq, Ord, Generic)
        deriving newtype (Num, Integral, PersistField, PersistFieldSql, PathPiece, ToHttpApiData, FromHttpApiData, Real, Enum, Bounded, A.ToJSON, A.FromJSON)

instance BackendCompatible SqlBackend SqlBackend where
    projectBackend = id

instance BackendCompatible SqlBackend SqlReadBackend where
    projectBackend = unSqlReadBackend

instance BackendCompatible SqlBackend SqlWriteBackend where
    projectBackend = unSqlWriteBackend

instance PersistStoreWrite SqlBackend where
    update _ [] = return ()
    update k upds = do
        conn <- ask
        let wher = whereStmtForKey conn k
        let sql = T.concat
                [ "UPDATE "
                , connEscapeTableName conn (entityDef $ Just $ recordTypeFromKey k)
                , " SET "
                , T.intercalate "," $ map (mkUpdateText conn) upds
                , " WHERE "
                , wher
                ]
        rawExecute sql $
            map updatePersistValue upds `mappend` keyToValues k

    insert val = do
        conn <- ask
        let esql = connInsertSql conn t vals
        key <-
            case esql of
                ISRSingle sql -> withRawQuery sql vals $ do
                    x <- CL.head
                    case x of
                        Just [PersistInt64 i] -> case keyFromValues [PersistInt64 i] of
                            Left err -> error $ "SQL insert: keyFromValues: PersistInt64 " `mappend` show i `mappend` " " `mappend` unpack err
                            Right k -> return k
                        Nothing -> error $ "SQL insert did not return a result giving the generated ID"
                        Just vals' -> case keyFromValues vals' of
                            Left e -> error $ "Invalid result from a SQL insert, got: " ++ show vals' ++ ". Error was: " ++ unpack e
                            Right k -> return k

                ISRInsertGet sql1 sql2 -> do
                    rawExecute sql1 vals
                    withRawQuery sql2 [] $ do
                        mm <- CL.head
                        let m = maybe
                                  (Left $ "No results from ISRInsertGet: " `mappend` tshow (sql1, sql2))
                                  Right mm

                        -- TODO: figure out something better for MySQL
                        let convert x =
                                case x of
                                    [PersistByteString i] -> case readInteger i of -- mssql
                                                            Just (ret,"") -> [PersistInt64 $ fromIntegral ret]
                                                            _ -> x
                                    _ -> x
                            -- Yes, it's just <|>. Older bases don't have the
                            -- instance for Either.
                            onLeft Left{} x = x
                            onLeft x _ = x

                        case m >>= (\x -> keyFromValues x `onLeft` keyFromValues (convert x)) of
                            Right k -> return k
                            Left err -> throw $ "ISRInsertGet: keyFromValues failed: " `mappend` err
                ISRManyKeys sql fs -> do
                    rawExecute sql vals
                    case entityPrimary t of
                       Nothing ->
                           error $ "ISRManyKeys is used when Primary is defined " ++ show sql
                       Just pdef ->
                            let pks = Foldable.toList $ fmap fieldHaskell $ compositeFields pdef
                                keyvals = map snd $ filter (\(a, _) -> let ret=isJust (find (== a) pks) in ret) $ zip (map fieldHaskell $ getEntityFields t) fs
                            in  case keyFromValues keyvals of
                                    Right k -> return k
                                    Left e  -> error $ "ISRManyKeys: unexpected keyvals result: " `mappend` unpack e

        return key
      where
        tshow :: Show a => a -> Text
        tshow = T.pack . show
        throw = liftIO . throwIO . userError . T.unpack
        t = entityDef $ Just val
        vals = mkInsertValues val

    insertMany [] = return []
    insertMany vals = do
        conn <- ask

        case connInsertManySql conn of
            Nothing -> mapM insert vals
            Just insertManyFn ->
                case insertManyFn ent valss of
                    ISRSingle sql -> rawSql sql (concat valss)
                    _ -> error "ISRSingle is expected from the connInsertManySql function"
                where
                    ent = entityDef vals
                    valss = map mkInsertValues vals

    insertMany_ vals0 = runChunked (length $ getEntityFields t) insertMany_' vals0
      where
        t = entityDef vals0
        insertMany_' vals = do
          conn <- ask
          let valss = map mkInsertValues vals
          let sql = T.concat
                  [ "INSERT INTO "
                  , connEscapeTableName conn t
                  , "("
                  , T.intercalate "," $ map (connEscapeFieldName conn . fieldDB) $ getEntityFields t
                  , ") VALUES ("
                  , T.intercalate "),(" $ replicate (length valss) $ T.intercalate "," $ map (const "?") (getEntityFields t)
                  , ")"
                  ]
          rawExecute sql (concat valss)

    replace k val = do
        conn <- ask
        let t = entityDef $ Just val
        let wher = whereStmtForKey conn k
        let sql = T.concat
                [ "UPDATE "
                , connEscapeTableName conn t
                , " SET "
                , T.intercalate "," (map (go conn . fieldDB) $ getEntityFields t)
                , " WHERE "
                , wher
                ]
            vals = mkInsertValues val `mappend` keyToValues k
        rawExecute sql vals
      where
        go conn x = connEscapeFieldName conn x `T.append` "=?"

    insertKey k v = insrepHelper "INSERT" [Entity k v]

    insertEntityMany es' = do
        conn <- ask
        let entDef = entityDef $ map entityVal es'
        let columnNames = keyAndEntityColumnNames entDef conn
        runChunked (length columnNames) go es'
      where
        go = insrepHelper "INSERT"

    repsert key value = do
        mExisting <- get key
        case mExisting of
          Nothing -> insertKey key value
          Just _ -> replace key value

    repsertMany [] = return ()
    repsertMany krsDups = do
        conn <- ask
        let krs = nubBy ((==) `on` fst) (reverse krsDups)
        let rs = snd `fmap` krs
        let ent = entityDef rs
        let nr  = length krs
        let toVals (k,r)
                = case entityPrimary ent of
                    Nothing -> keyToValues k <> (mkInsertValues r)
                    Just _  -> mkInsertValues r
        case connRepsertManySql conn of
            (Just mkSql) -> rawExecute (mkSql ent nr) (concatMap toVals krs)
            Nothing -> mapM_ (uncurry repsert) krs

    delete k = do
        conn <- ask
        rawExecute (sql conn) (keyToValues k)
      where
        wher conn = whereStmtForKey conn k
        sql conn = T.concat
            [ "DELETE FROM "
            , connEscapeTableName conn (entityDef $ Just $ recordTypeFromKey k)
            , " WHERE "
            , wher conn
            ]
instance PersistStoreWrite SqlWriteBackend where
    insert v = withBaseBackend $ insert v
    insertMany vs = withBaseBackend $ insertMany vs
    insertMany_ vs = withBaseBackend $ insertMany_ vs
    insertEntityMany vs = withBaseBackend $ insertEntityMany vs
    insertKey k v = withBaseBackend $ insertKey k v
    repsert k v = withBaseBackend $ repsert k v
    replace k v = withBaseBackend $ replace k v
    delete k = withBaseBackend $ delete k
    update k upds = withBaseBackend $ update k upds
    repsertMany krs = withBaseBackend $ repsertMany krs

instance PersistStoreRead SqlBackend where
    get k = do
        mEs <- getMany [k]
        return $ Map.lookup k mEs

    -- inspired by Database.Persist.Sql.Orphan.PersistQuery.selectSourceRes
    getMany []      = return Map.empty
    getMany ks@(k:_)= do
        conn <- ask
        let t = entityDef . dummyFromKey $ k
        let cols = commaSeparated . Foldable.toList . keyAndEntityColumnNames t
        let wher = whereStmtForKeys conn ks
        let sql = T.concat
                [ "SELECT "
                , cols conn
                , " FROM "
                , connEscapeTableName conn t
                , " WHERE "
                , wher
                ]
        let parse vals
                = case parseEntityValues t vals of
                    Left s -> liftIO $ throwIO $
                        PersistMarshalError ("getBy: " <> s)
                    Right row -> return row
        withRawQuery sql (Foldable.foldMap keyToValues ks) $ do
            es <- CL.mapM parse .| CL.consume
            return $ Map.fromList $ fmap (\e -> (entityKey e, entityVal e)) es

instance PersistStoreRead SqlReadBackend where
    get k = withBaseBackend $ get k
    getMany ks = withBaseBackend $ getMany ks
instance PersistStoreRead SqlWriteBackend where
    get k = withBaseBackend $ get k
    getMany ks = withBaseBackend $ getMany ks

dummyFromKey :: Key record -> Maybe record
dummyFromKey = Just . recordTypeFromKey

recordTypeFromKey :: Key record -> record
recordTypeFromKey _ = error "dummyFromKey"

insrepHelper :: (MonadIO m, PersistEntity val)
             => Text
             -> [Entity val]
             -> ReaderT SqlBackend m ()
insrepHelper _       []  = return ()
insrepHelper command es = do
    conn <- ask
    let columnNames = Foldable.toList $ keyAndEntityColumnNames entDef conn
    rawExecute (sql conn columnNames) vals
  where
    entDef = entityDef $ map entityVal es
    sql conn columnNames = T.concat
        [ command
        , " INTO "
        , connEscapeTableName conn entDef
        , "("
        , T.intercalate "," columnNames
        , ") VALUES ("
        , T.intercalate "),(" $ replicate (length es) $ T.intercalate "," $ fmap (const "?") columnNames
        , ")"
        ]
    vals = Foldable.foldMap entityValues es

runChunked
    :: (Monad m)
    => Int
    -> ([a] -> ReaderT SqlBackend m ())
    -> [a]
    -> ReaderT SqlBackend m ()
runChunked _ _ []     = return ()
runChunked width m xs = do
    conn <- ask
    case connMaxParams conn of
        Nothing -> m xs
        Just maxParams -> let chunkSize = maxParams `div` width in
            mapM_ m (chunksOf chunkSize xs)

-- Implement this here to avoid depending on the split package
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size xs = let (chunk, rest) = splitAt size xs in chunk : chunksOf size rest
