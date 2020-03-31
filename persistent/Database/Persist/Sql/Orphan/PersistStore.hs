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
import Control.Monad.Trans.Reader (ReaderT, ask, withReaderT)
import Data.Acquire (with)
import qualified Data.Aeson as A
import Data.ByteString.Char8 (readInteger)
import Data.Conduit (ConduitM, (.|), runConduit)
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as Foldable
import Data.Function (on)
import Data.Int (Int64)
import Data.List (find, nubBy)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Monoid (mappend, (<>))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Void (Void)
import Web.PathPieces (PathPiece)
import Web.HttpApiData (ToHttpApiData, FromHttpApiData)

import Database.Persist
import Database.Persist.Class ()
import Database.Persist.Sql.Class (PersistFieldSql)
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Types
import Database.Persist.Sql.Util (
    dbIdColumns, keyAndEntityColumnNames, parseEntityValues, entityColumnNames
  , updatePersistValue, mkUpdateText, commaSeparated)

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
  $ map (<> "=? ")
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
getTableName rec = withReaderT projectBackend $ do
    conn <- ask
    return $ connEscapeName conn $ tableDBName rec

-- | useful for a backend to implement tableName by adding escaping
tableDBName :: (PersistEntity record) => record -> DBName
tableDBName rec = entityDB $ entityDef (Just rec)

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
getFieldName rec = withReaderT projectBackend $ do
    conn <- ask
    return $ connEscapeName conn $ fieldDBName rec

-- | useful for a backend to implement fieldName by adding escaping
fieldDBName :: forall record typ. (PersistEntity record) => EntityField record typ -> DBName
fieldDBName = fieldDB . persistFieldDef


instance PersistCore SqlBackend where
    newtype BackendKey SqlBackend = SqlBackendKey { unSqlBackendKey :: Int64 }
        deriving (Show, Read, Eq, Ord, Num, Integral, PersistField, PersistFieldSql, PathPiece, ToHttpApiData, FromHttpApiData, Real, Enum, Bounded, A.ToJSON, A.FromJSON)
instance PersistCore SqlReadBackend where
    newtype BackendKey SqlReadBackend = SqlReadBackendKey { unSqlReadBackendKey :: Int64 }
        deriving (Show, Read, Eq, Ord, Num, Integral, PersistField, PersistFieldSql, PathPiece, ToHttpApiData, FromHttpApiData, Real, Enum, Bounded, A.ToJSON, A.FromJSON)
instance PersistCore SqlWriteBackend where
    newtype BackendKey SqlWriteBackend = SqlWriteBackendKey { unSqlWriteBackendKey :: Int64 }
        deriving (Show, Read, Eq, Ord, Num, Integral, PersistField, PersistFieldSql, PathPiece, ToHttpApiData, FromHttpApiData, Real, Enum, Bounded, A.ToJSON, A.FromJSON)

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
                , connEscapeName conn $ tableDBName $ recordTypeFromKey k
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
                       Nothing -> error $ "ISRManyKeys is used when Primary is defined " ++ show sql
                       Just pdef ->
                            let pks = map fieldHaskell $ compositeFields pdef
                                keyvals = map snd $ filter (\(a, _) -> let ret=isJust (find (== a) pks) in ret) $ zip (map fieldHaskell $ entityFields t) fs
                            in  case keyFromValues keyvals of
                                    Right k -> return k
                                    Left e  -> error $ "ISRManyKeys: unexpected keyvals result: " `mappend` unpack e

        return key
      where
        tshow :: Show a => a -> Text
        tshow = T.pack . show
        throw = liftIO . throwIO . userError . T.unpack
        t = entityDef $ Just val
        vals = map toPersistValue $ toPersistFields val

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
                    valss = map (map toPersistValue . toPersistFields) vals

    insertMany_ vals0 = runChunked (length $ entityFields t) insertMany_' vals0
      where
        t = entityDef vals0
        insertMany_' vals = do
          conn <- ask
          let valss = map (map toPersistValue . toPersistFields) vals
          let sql = T.concat
                  [ "INSERT INTO "
                  , connEscapeName conn (entityDB t)
                  , "("
                  , T.intercalate "," $ map (connEscapeName conn . fieldDB) $ entityFields t
                  , ") VALUES ("
                  , T.intercalate "),(" $ replicate (length valss) $ T.intercalate "," $ map (const "?") (entityFields t)
                  , ")"
                  ]
          rawExecute sql (concat valss)

    replace k val = do
        conn <- ask
        let t = entityDef $ Just val
        let wher = whereStmtForKey conn k
        let sql = T.concat
                [ "UPDATE "
                , connEscapeName conn (entityDB t)
                , " SET "
                , T.intercalate "," (map (go conn . fieldDB) $ entityFields t)
                , " WHERE "
                , wher
                ]
            vals = map toPersistValue (toPersistFields val) `mappend` keyToValues k
        rawExecute sql vals
      where
        go conn x = connEscapeName conn x `T.append` "=?"

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
                    Nothing -> keyToValues k <> (toPersistValue <$> toPersistFields r)
                    Just _  -> toPersistValue <$> toPersistFields r
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
            , connEscapeName conn $ tableDBName $ recordTypeFromKey k
            , " WHERE "
            , wher conn
            ]
instance PersistStoreWrite SqlWriteBackend where
    insert v = withReaderT persistBackend $ insert v
    insertMany vs = withReaderT persistBackend $ insertMany vs
    insertMany_ vs = withReaderT persistBackend $ insertMany_ vs
    insertEntityMany vs = withReaderT persistBackend $ insertEntityMany vs
    insertKey k v = withReaderT persistBackend $ insertKey k v
    repsert k v = withReaderT persistBackend $ repsert k v
    replace k v = withReaderT persistBackend $ replace k v
    delete k = withReaderT persistBackend $ delete k
    update k upds = withReaderT persistBackend $ update k upds
    repsertMany krs = withReaderT persistBackend $ repsertMany krs

instance PersistStoreRead SqlBackend where
    get k = do
        mEs <- getMany [k]
        return $ Map.lookup k mEs

    -- inspired by Database.Persist.Sql.Orphan.PersistQuery.selectSourceRes
    getMany []      = return Map.empty
    getMany ks@(k:_)= do
        conn <- ask
        let t = entityDef . dummyFromKey $ k
        let cols = commaSeparated . entityColumnNames t
        let wher = whereStmtForKeys conn ks
        let sql = T.concat
                [ "SELECT "
                , cols conn
                , " FROM "
                , connEscapeName conn $ entityDB t
                , " WHERE "
                , wher
                ]
        let parse vals
                = case parseEntityValues t vals of
                    Left s -> liftIO $ throwIO $ PersistMarshalError s
                    Right row -> return row
        withRawQuery sql (Foldable.foldMap keyToValues ks) $ do
            es <- CL.mapM parse .| CL.consume
            return $ Map.fromList $ fmap (\e -> (entityKey e, entityVal e)) es

instance PersistStoreRead SqlReadBackend where
    get k = withReaderT persistBackend $ get k
    getMany ks = withReaderT persistBackend $ getMany ks
instance PersistStoreRead SqlWriteBackend where
    get k = withReaderT persistBackend $ get k
    getMany ks = withReaderT persistBackend $ getMany ks

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
    let columnNames = keyAndEntityColumnNames entDef conn
    rawExecute (sql conn columnNames) vals
  where
    entDef = entityDef $ map entityVal es
    sql conn columnNames = T.concat
        [ command
        , " INTO "
        , connEscapeName conn (entityDB entDef)
        , "("
        , T.intercalate "," columnNames
        , ") VALUES ("
        , T.intercalate "),(" $ replicate (length es) $ T.intercalate "," $ map (const "?") columnNames
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
