-- | Use persistent-mongodb the same way you would use other persistent
-- libraries and refer to the general persistent documentation.
-- There are some new MongoDB specific filters under the filters section.
-- These help extend your query into a nested document.
--
-- However, at some point you will find the normal Persistent APIs lacking.
-- and want lower level-level MongoDB access.
-- There are functions available to make working with the raw driver
-- easier: they are under the Entity conversion section.
-- You should still use the same connection pool that you are using for Persistent. 
--
-- MongoDB is a schema-less database.
-- The MongoDB Persistent backend does not help perform migrations.
-- Unlike SQL backends, uniqueness constraints cannot be created for you.
-- You must place a unique index on unique fields.
{-# LANGUAGE CPP, PackageImports, OverloadedStrings, ScopedTypeVariables  #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE RankNTypes, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}

{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Database.Persist.MongoDB
    (
    -- * Entity conversion
      collectionName
    , docToEntityEither
    , docToEntityThrow
    , entityToDocument
    , toInsertDoc
    , updatesToDoc
    , filtersToDoc
    , toUniquesDoc

    -- * MongoDB specific Filters
    -- $filters
    , nestEq, nestNe, nestGe, nestLe, nestIn, nestNotIn
    , anyEq, multiEq, nestBsonEq, anyBsonEq, multiBsonEq
    , (=~.), (?=~.), MongoRegex
    , (->.), (~>.), (?&->.), (?&~>.), (&->.), (&~>.)
    -- non-operator forms of filters
    , NestedField(..)
    , MongoRegexSearchable

    -- * Key conversion helpers
    , BackendKey(..)
    , keyToOid
    , oidToKey
    , recordTypeFromKey

    -- * PersistField conversion
    , fieldName

    -- * using connections
    , withConnection
    , withMongoPool
    , withMongoDBConn
    , withMongoDBPool
    , createMongoDBPool
    , runMongoDBPool
    , runMongoDBPoolDef
    , ConnectionPool
    , Connection
    , MongoBackend
    , MongoAuth (..)

    -- * Connection configuration
    , MongoConf (..)
    , defaultMongoConf
    , defaultHost
    , defaultAccessMode
    , defaultPoolStripes
    , defaultConnectionIdleTime
    , defaultStripeConnections
    , applyDockerEnv

    -- ** using raw MongoDB pipes
    , PipePool
    , createMongoDBPipePool
    , runMongoDBPipePool

    -- * network type
    , HostName
    , PortID

    -- * MongoDB driver types
    , Database
    , DB.Action
    , DB.AccessMode(..)
    , DB.master
    , DB.slaveOk
    , (DB.=:)
    , DB.ObjectId

    -- * Database.Persist
    , module Database.Persist
    ) where

import Database.Persist
import qualified Database.Persist.Sql as Sql

import qualified Control.Monad.IO.Class as Trans
import Control.Exception (throw, throwIO)

import Data.Bson (ObjectId(..))
import qualified Database.MongoDB as DB
import Database.MongoDB.Query (Database)
import Control.Applicative (Applicative)
import Network (PortID (PortNumber))
import Network.Socket (HostName)
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E
import qualified Data.Serialize as Serialize
import Web.PathPieces (PathPiece (..))
import Data.Conduit
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Object, Number), (.:), (.:?), (.!=), FromJSON(..), ToJSON(..), withText)
import Control.Monad (mzero, liftM)
import qualified Data.Conduit.Pool as Pool
import Data.Time (NominalDiffTime)
#ifdef HIGH_PRECISION_DATE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
#endif
import Data.Time.Calendar (Day(..))
#if MIN_VERSION_aeson(0, 7, 0)
#else
import Data.Attoparsec.Number
#endif
import Data.Bits (shiftR)
import Data.Word (Word16)
import Data.Monoid (mappend)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Numeric (readHex)

#if MIN_VERSION_base(4,6,0)
import System.Environment (lookupEnv)
#else
import System.Environment (getEnvironment)
#endif

#ifdef DEBUG
import FileLocation (debug)
#endif

#if !MIN_VERSION_base(4,6,0)
lookupEnv :: String -> IO (Maybe String)
lookupEnv key = do
    env <- getEnvironment
    return $ lookup key env
#endif

instance HasPersistBackend MongoBackend MongoBackend where
    persistBackend = id

recordTypeFromKey :: Key record -> record
recordTypeFromKey _ = error "recordTypeFromKey"

newtype NoOrphanNominalDiffTime = NoOrphanNominalDiffTime NominalDiffTime
                                deriving (Show, Eq, Num)

instance FromJSON NoOrphanNominalDiffTime where
#if MIN_VERSION_aeson(0, 7, 0)
    parseJSON (Number x) = (return . NoOrphanNominalDiffTime . fromRational . toRational) x


#else 
    parseJSON (Number (I x)) = (return . NoOrphanNominalDiffTime . fromInteger) x
    parseJSON (Number (D x)) = (return . NoOrphanNominalDiffTime . fromRational . toRational) x

#endif                           
    parseJSON _ = fail "couldn't parse diff time"

newtype NoOrphanPortID = NoOrphanPortID PortID deriving (Show, Eq)


instance FromJSON NoOrphanPortID where
#if MIN_VERSION_aeson(0, 7, 0)  
    parseJSON (Number  x) = (return . NoOrphanPortID . PortNumber . fromIntegral ) cnvX
      where cnvX :: Word16
            cnvX = round x 

#else
    parseJSON (Number (I x)) = (return . NoOrphanPortID . PortNumber . fromInteger) x

#endif
    parseJSON _ = fail "couldn't parse port number"


data Connection = Connection DB.Pipe DB.Database
type ConnectionPool = Pool.Pool Connection

-- | ToPathPiece is used to convert a key to/from text
instance PathPiece (BackendKey MongoBackend) where
    toPathPiece = keyToText
    fromPathPiece keyText = readMayKey $
        -- handle a JSON type prefix
        -- 'o' is a non-hex character, so no confusion here
        case T.uncons keyText of
            Just ('o', prefixed) -> prefixed
            _ -> keyText

keyToText :: BackendKey MongoBackend -> Text
keyToText = T.pack . show . unMongoBackendKey

-- | Convert a Text to a Key
readMayKey :: Text -> Maybe (BackendKey MongoBackend)
readMayKey str =
  case filter (null . snd) $ reads $ T.unpack str :: [(DB.ObjectId,String)] of
    (parsed,_):[] -> Just $ MongoBackendKey parsed
    _ -> Nothing

instance PersistField DB.ObjectId where
    toPersistValue = oidToPersistValue
    fromPersistValue oid@(PersistObjectId _) = Right $ persistObjectIdToDbOid oid
    fromPersistValue (PersistByteString bs) = fromPersistValue (PersistObjectId bs)
    fromPersistValue _ = Left $ T.pack "expected PersistObjectId"

instance Sql.PersistFieldSql DB.ObjectId where
    sqlType _ = Sql.SqlOther "doesn't make much sense for MongoDB"

instance Sql.PersistFieldSql (BackendKey MongoBackend) where
    sqlType _ = Sql.SqlOther "doesn't make much sense for MongoDB"


withConnection :: (Trans.MonadIO m, Applicative m)
               => MongoConf
               -> (ConnectionPool -> m b) -> m b
withConnection mc =
  withMongoDBPool (mgDatabase mc) (T.unpack $ mgHost mc) (mgPort mc) (mgAuth mc) (mgPoolStripes mc) (mgStripeConnections mc) (mgConnectionIdleTime mc)

withMongoDBConn :: (Trans.MonadIO m, Applicative m)
                => Database -> HostName -> PortID
                -> Maybe MongoAuth -> NominalDiffTime
                -> (ConnectionPool -> m b) -> m b
withMongoDBConn dbname hostname port mauth connectionIdleTime = withMongoDBPool dbname hostname port mauth 1 1 connectionIdleTime

createPipe :: HostName -> PortID -> IO DB.Pipe
createPipe hostname port = DB.connect (DB.Host hostname port)

createReplicatSet :: (DB.ReplicaSetName, [DB.Host]) -> Database -> Maybe MongoAuth -> IO Connection
createReplicatSet rsSeed dbname mAuth = do
    pipe <- DB.openReplicaSet rsSeed >>= DB.primary
    testAccess pipe dbname mAuth
    return $ Connection pipe dbname

createRsPool :: (Trans.MonadIO m, Applicative m) => Database -> ReplicaSetConfig
              -> Maybe MongoAuth
              -> Int -- ^ pool size (number of stripes)
              -> Int -- ^ stripe size (number of connections per stripe)
              -> NominalDiffTime -- ^ time a connection is left idle before closing
              -> m ConnectionPool
createRsPool dbname (ReplicaSetConfig rsName rsHosts) mAuth connectionPoolSize stripeSize connectionIdleTime = do
    Trans.liftIO $ Pool.createPool
                          (createReplicatSet (rsName, rsHosts) dbname mAuth)
                          (\(Connection pipe _) -> DB.close pipe)
                          connectionPoolSize
                          connectionIdleTime
                          stripeSize

testAccess :: DB.Pipe -> Database -> Maybe MongoAuth -> IO ()
testAccess pipe dbname mAuth = do
    _ <- case mAuth of
      Just (MongoAuth user pass) -> DB.access pipe DB.UnconfirmedWrites dbname (DB.auth user pass)
      Nothing -> return undefined
    return ()

createConnection :: Database -> HostName -> PortID -> Maybe MongoAuth -> IO Connection
createConnection dbname hostname port mAuth = do
    pipe <- createPipe hostname port
    testAccess pipe dbname mAuth
    return $ Connection pipe dbname

createMongoDBPool :: (Trans.MonadIO m, Applicative m) => Database -> HostName -> PortID
                  -> Maybe MongoAuth
                  -> Int -- ^ pool size (number of stripes)
                  -> Int -- ^ stripe size (number of connections per stripe)
                  -> NominalDiffTime -- ^ time a connection is left idle before closing
                  -> m ConnectionPool
createMongoDBPool dbname hostname port mAuth connectionPoolSize stripeSize connectionIdleTime = do
  Trans.liftIO $ Pool.createPool
                          (createConnection dbname hostname port mAuth)
                          (\(Connection pipe _) -> DB.close pipe)
                          connectionPoolSize
                          connectionIdleTime
                          stripeSize


createMongoPool :: (Trans.MonadIO m, Applicative m) => MongoConf -> m ConnectionPool
createMongoPool c@MongoConf{mgReplicaSetConfig = Just (ReplicaSetConfig rsName hosts)} =
      createRsPool
         (mgDatabase c)
         (ReplicaSetConfig rsName ((DB.Host (T.unpack $ mgHost c) (mgPort c)):hosts))
         (mgAuth c)
         (mgPoolStripes c) (mgStripeConnections c) (mgConnectionIdleTime c)
createMongoPool c@MongoConf{mgReplicaSetConfig = Nothing} =
      createMongoDBPool
         (mgDatabase c) (T.unpack (mgHost c)) (mgPort c)
         (mgAuth c)
         (mgPoolStripes c) (mgStripeConnections c) (mgConnectionIdleTime c)

type PipePool = Pool.Pool DB.Pipe

-- | A pool of plain MongoDB pipes.
-- The database parameter has not yet been applied yet.
-- This is useful for switching between databases (on the same host and port)
-- Unlike the normal pool, no authentication is available
createMongoDBPipePool :: (Trans.MonadIO m, Applicative m) => HostName -> PortID
                  -> Int -- ^ pool size (number of stripes)
                  -> Int -- ^ stripe size (number of connections per stripe)
                  -> NominalDiffTime -- ^ time a connection is left idle before closing
                  -> m PipePool
createMongoDBPipePool hostname port connectionPoolSize stripeSize connectionIdleTime =
  Trans.liftIO $ Pool.createPool
                          (createPipe hostname port)
                          DB.close
                          connectionPoolSize
                          connectionIdleTime
                          stripeSize

withMongoPool :: (Trans.MonadIO m, Applicative m) => MongoConf -> (ConnectionPool -> m b) -> m b
withMongoPool conf connectionReader = createMongoPool conf >>= connectionReader

withMongoDBPool :: (Trans.MonadIO m, Applicative m) =>
  Database -> HostName -> PortID -> Maybe MongoAuth -> Int -> Int -> NominalDiffTime -> (ConnectionPool -> m b) -> m b
withMongoDBPool dbname hostname port mauth poolStripes stripeConnections connectionIdleTime connectionReader = do
  pool <- createMongoDBPool dbname hostname port mauth poolStripes stripeConnections connectionIdleTime
  connectionReader pool

-- | run a pool created with 'createMongoDBPipePool'
runMongoDBPipePool :: (Trans.MonadIO m, MonadBaseControl IO m) => DB.AccessMode -> Database -> DB.Action m a -> PipePool -> m a
runMongoDBPipePool accessMode db action pool =
  Pool.withResource pool $ \pipe -> DB.access pipe accessMode db action

runMongoDBPool :: (Trans.MonadIO m, MonadBaseControl IO m) => DB.AccessMode  -> DB.Action m a -> ConnectionPool -> m a
runMongoDBPool accessMode action pool =
  Pool.withResource pool $ \(Connection pipe db) -> DB.access pipe accessMode db action


-- | use default 'AccessMode'
runMongoDBPoolDef :: (Trans.MonadIO m, MonadBaseControl IO m) => DB.Action m a -> ConnectionPool -> m a
runMongoDBPoolDef = runMongoDBPool (DB.ConfirmWrites ["j" DB.=: True])

queryByKey :: (PersistEntity record, PersistEntityBackend record ~ MongoBackend)
           => Key record -> DB.Query
queryByKey k = DB.select (keyToMongoDoc k) (collectionNameFromKey k)

selectByKey :: (PersistEntity record, PersistEntityBackend record ~ MongoBackend)
            => Key record -> DB.Selection
selectByKey k = DB.select (keyToMongoDoc k) (collectionNameFromKey k)

updatesToDoc :: (PersistEntity entity) => [Update entity] -> DB.Document
updatesToDoc upds = map updateToMongoField upds

updateToMongoField :: (PersistEntity entity) => Update entity -> DB.Field
updateToMongoField (Update field v up) =
    opName DB.:= DB.Doc [fieldName field DB.:= opValue]
    where 
      inc = "$inc"
      mul = "$mul"
      (opName, opValue) =
        case (up, toPersistValue v) of
                  (Assign, PersistNull) -> ("$unset", DB.Int64 1)
                  (Assign,a)    -> ("$set", DB.val a)
                  (Add, a)      -> (inc, DB.val a)
                  (Subtract, PersistInt64 i) -> (inc, DB.Int64 (-i))
                  (Multiply, PersistInt64 i) -> (mul, DB.Int64 i)
                  (Multiply, PersistDouble d) -> (mul, DB.Float d)
                  (Subtract, _) -> error "expected PersistInt64 for a subtraction"
                  (Multiply, _) -> error "expected PersistInt64 or PersistDouble for a subtraction"
                  -- Obviously this could be supported for floats by multiplying with 1/x
                  (Divide, _)   -> throw $ PersistMongoDBUnsupported "divide not supported"
updateToMongoField (BackendUpdate _) = error "no backend updates implemented yet"


-- | convert a unique key into a MongoDB document
toUniquesDoc :: forall record. (PersistEntity record) => Unique record -> [DB.Field]
toUniquesDoc uniq = zipWith (DB.:=)
  (map (unDBName . snd) $ persistUniqueToFieldNames uniq)
  (map DB.val (persistUniqueToValues uniq))

-- | convert a PersistEntity into document fields.
-- for inserts only: nulls are ignored so they will be unset in the document.
-- 'entityToDocument' includes nulls
toInsertDoc :: forall record.  (PersistEntity record, PersistEntityBackend record ~ MongoBackend)
            => record -> DB.Document
toInsertDoc record = zipFilter (embeddedFields $ toEmbeddedDef entDef)
    (map toPersistValue $ toPersistFields record)
  where
    entDef = entityDef $ Just record
    zipFilter :: [EmbeddedFieldDef] -> [PersistValue] -> DB.Document
    zipFilter [] _  = []
    zipFilter _  [] = []
    zipFilter (fd:efields) (pv:pvs) =
        if isNull pv then recur else
          (fieldToLabel fd DB.:= embeddedVal (emFieldEmbedded fd) pv):recur

      where
        recur = zipFilter efields pvs

        isNull PersistNull = True
        isNull (PersistMap m) = null m
        isNull (PersistList l) = null l
        isNull _ = False

    -- make sure to removed nulls from embedded entities also
    embeddedVal :: Maybe EmbeddedDef -> PersistValue -> DB.Value
    embeddedVal (Just emDef) (PersistMap m) = DB.Doc $
      zipFilter (embeddedFields emDef) $ map snd m
    embeddedVal je@(Just _) (PersistList l) = DB.Array $ map (embeddedVal je) l
    embeddedVal _ pv = DB.val pv

collectionName :: (PersistEntity record, PersistEntityBackend record ~ MongoBackend)
               => record -> Text
collectionName = unDBName . entityDB . entityDef . Just

-- | convert a PersistEntity into document fields.
-- unlike 'toInsertDoc', nulls are included.
entityToDocument :: (PersistEntity record, PersistEntityBackend record ~ MongoBackend)
                 => record -> DB.Document
entityToDocument record = zipToDoc (map fieldDB $ entityFields entity) (toPersistFields record)
  where
    entity = entityDef $ Just record

zipToDoc :: PersistField a => [DBName] -> [a] -> [DB.Field]
zipToDoc [] _  = []
zipToDoc _  [] = []
zipToDoc (e:efields) (p:pfields) =
  let pv = toPersistValue p
  in  (unDBName e DB.:= DB.val pv):zipToDoc efields pfields

fieldToLabel :: EmbeddedFieldDef -> Text
fieldToLabel = unDBName . emFieldDB

saveWithKey :: forall m record.
            (PersistEntity record, PersistEntityBackend record ~ MongoBackend)
            => (record -> [DB.Field])
            -> (Text -> [DB.Field] -> DB.Action m ())
            -> Key record
            -> record
            -> DB.Action m ()
saveWithKey entToFields dbSave key record =
      dbSave (collectionName record) ((keyToMongoDoc key) ++ (entToFields record))

type MongoBackend = DB.MongoContext -- FIXME

keyFrom_idEx :: (Trans.MonadIO m, PersistEntity record) => DB.Value -> m (Key record)
keyFrom_idEx idVal = case keyFrom_id idVal of
    Right k  -> return k
    Left err -> liftIO $ throwIO $ PersistMongoDBError $ "could not convert key: "
        `mappend` T.pack (show idVal)
        `mappend` err

keyFrom_id :: (PersistEntity record) => DB.Value -> Either Text (Key record)
keyFrom_id idVal = case cast idVal of
    (PersistMap m) -> keyFromValues $ map snd m
    pv -> keyFromValues [pv]

-- | It would make sense to define the instance for ObjectId
-- and then use newtype deriving
-- however, that would create an orphan instance
instance ToJSON (BackendKey MongoBackend) where
    toJSON (MongoBackendKey (Oid x y)) = toJSON $ DB.showHexLen 8 x $ DB.showHexLen 16 y ""

instance FromJSON (BackendKey MongoBackend) where
    parseJSON = withText "BackendKey MongoBackend" $ \t ->
        maybe
          (fail "Invalid base64")
          (return . MongoBackendKey . persistObjectIdToDbOid . PersistObjectId)
          $ fmap (i2bs (8 * 12) . fst) $ headMay $ readHex $ T.unpack t
      where
        -- should these be exported from Types/Base.hs ?
        headMay []    = Nothing
        headMay (x:_) = Just x

        -- taken from crypto-api
        -- |@i2bs bitLen i@ converts @i@ to a 'ByteString' of @bitLen@ bits (must be a multiple of 8).
        i2bs :: Int -> Integer -> BS.ByteString
        i2bs l i = BS.unfoldr (\l' -> if l' < 0 then Nothing else Just (fromIntegral (i `shiftR` l'), l' - 8)) (l-8)
        {-# INLINE i2bs #-}

instance PersistStore DB.MongoContext where
    newtype BackendKey MongoBackend = MongoBackendKey { unMongoBackendKey :: DB.ObjectId }
        deriving (Show, Read, Eq, Ord, PersistField)

    backendKeyToValues (MongoBackendKey oid)   = [oidToPersistValue oid]
    backendKeyFromValues [poid@(PersistObjectId _)] =
        Right $ MongoBackendKey $ persistObjectIdToDbOid poid
    backendKeyFromValues s = Left $ T.pack $ show s

    insert record = do
        keyFrom_idEx =<< DB.insert (collectionName record) (toInsertDoc record)

    insertMany [] = return []
    insertMany (r:records) = mapM keyFrom_idEx =<<
        DB.insertMany (collectionName r) (map toInsertDoc (r:records))

    insertKey k record = saveWithKey toInsertDoc DB.insert_ k record

    repsert   k record = saveWithKey entityToDocument DB.save k record

    replace k record = do
        DB.replace (selectByKey k) (entityToDocument record)
        return ()

    delete k =
        DB.deleteOne DB.Select {
          DB.coll = collectionNameFromKey k
        , DB.selector = keyToMongoDoc k
        }

    get k = do
            d <- DB.findOne (queryByKey k)
            case d of
              Nothing -> return Nothing
              Just doc -> do
                Entity _ ent <- fromPersistValuesThrow t doc
                return $ Just ent
          where
            t = entityDefFromKey k

    update _ [] = return ()
    update key upds =
        DB.modify
           (DB.Select (keyToMongoDoc key) (collectionNameFromKey key))
           $ updatesToDoc upds

    updateGet key upds = do
        result <- DB.findAndModify (DB.select (keyToMongoDoc key)
                     (collectionNameFromKey key)
                   ) (updatesToDoc upds)
        either err instantiate result
      where
        instantiate doc = do
            Entity _ rec <- fromPersistValuesThrow t doc
            return rec
        err msg = Trans.liftIO $ throwIO $ KeyNotFound $ show key ++ msg
        t = entityDefFromKey key


instance PersistUnique DB.MongoContext where
    getBy uniq = do
        mdoc <- DB.findOne $
          DB.select (toUniquesDoc uniq) (collectionName rec)
        case mdoc of
            Nothing -> return Nothing
            Just doc -> liftM Just $ fromPersistValuesThrow t doc
      where
        t = entityDef $ Just rec
        rec = dummyFromUnique uniq

    deleteBy uniq =
        DB.delete DB.Select {
          DB.coll = collectionName $ dummyFromUnique uniq
        , DB.selector = toUniquesDoc uniq
        }

    upsert newRecord upds = do
        uniq <- onlyUnique newRecord
        let uniqueDoc = toUniquesDoc uniq
        let uniqKeys = map DB.label uniqueDoc
        let insDoc = DB.exclude uniqKeys $ toInsertDoc newRecord
        let selection = DB.select uniqueDoc $ collectionName newRecord
        if null upds
          then DB.upsert selection ["$set" DB.=: insDoc]
          else do
            DB.upsert selection ["$setOnInsert" DB.=: insDoc]
            DB.modify selection $ updatesToDoc upds
        -- because findAndModify $setOnInsert is broken we do a separate get now
        mdoc <- getBy uniq
        maybe (err "possible race condition: getBy found Nothing")
            return mdoc
      where
        err = Trans.liftIO . throwIO . UpsertError
        {-
        -- cannot use findAndModify
        -- because $setOnInsert is crippled
        -- https://jira.mongodb.org/browse/SERVER-2643
        result <- DB.findAndModifyOpts
            selection
            (DB.defFamUpdateOpts ("$setOnInsert" DB.=: insDoc : ["$set" DB.=: insDoc]))
              { DB.famUpsert = True }
        either err instantiate result
      where
        -- this is only possible when new is False
        instantiate Nothing = error "upsert: impossible null"
        instantiate (Just doc) =
            fromPersistValuesThrow (entityDef $ Just newRecord) doc
            -}
   

_id :: T.Text
_id = "_id"

-- _id is always the primary key in MongoDB
-- but _id can contain any unique value
keyToMongoDoc :: (PersistEntity record, PersistEntityBackend record ~ MongoBackend)
                  => Key record -> DB.Document
keyToMongoDoc k = case entityPrimary $ entityDefFromKey k of
    Nothing   -> zipToDoc [DBName _id] values
    Just pdef -> [_id DB.=: zipToDoc (primaryNames pdef)  values]
  where
    primaryNames = map fieldDB . primaryFields
    values = keyToValues k

entityDefFromKey :: PersistEntity record => Key record => EntityDef
entityDefFromKey = entityDef . Just . recordTypeFromKey

collectionNameFromKey :: (PersistEntity record, PersistEntityBackend record ~ MongoBackend)
                      => Key record => Text
collectionNameFromKey = collectionName . recordTypeFromKey


instance PersistQuery DB.MongoContext where
    updateWhere _ [] = return ()
    updateWhere filts upds =
        DB.modify DB.Select {
          DB.coll = collectionName $ dummyFromFilts filts
        , DB.selector = filtersToDoc filts
        } $ updatesToDoc upds

    deleteWhere filts = do
        DB.delete DB.Select {
          DB.coll = collectionName $ dummyFromFilts filts
        , DB.selector = filtersToDoc filts
        }

    count filts = do
        i <- DB.count query
        return $ fromIntegral i
      where
        query = DB.select (filtersToDoc filts) $
                  collectionName $ dummyFromFilts filts

    selectSourceRes filts opts = do
        context <- ask
        let make = do
                cursor <- liftIO $ runReaderT (DB.find $ makeQuery filts opts) context
                pull context cursor
        return $ return make
      where
        pull context cursor = do
            mdoc <- liftIO $ runReaderT (DB.next cursor) context
            case mdoc of
                Nothing -> return ()
                Just doc -> do
                    entity <- fromPersistValuesThrow t doc
                    yield entity
                    pull context cursor
        t = entityDef $ Just $ dummyFromFilts filts

    selectFirst filts opts = do
        mdoc <- DB.findOne $ makeQuery filts opts
        case mdoc of
            Nothing -> return Nothing
            Just doc -> liftM Just $ fromPersistValuesThrow t doc
      where
        t = entityDef $ Just $ dummyFromFilts filts

    selectKeysRes filts opts = do
        context <- ask
        let make = do
                cursor <- liftIO $ flip runReaderT context $ DB.find $ (makeQuery filts opts) {
                    DB.project = [_id DB.=: (1 :: Int)]
                  }
                pull context cursor
        return $ return make
      where
        pull context cursor = do
            mdoc <- liftIO $ runReaderT (DB.next cursor) context
            case mdoc of
                Nothing -> return ()
                Just [_id DB.:= idVal] -> do
                    k <- liftIO $ keyFrom_idEx idVal
                    yield k
                    pull context cursor
                Just y -> liftIO $ throwIO $ PersistMarshalError $ T.pack $ "Unexpected in selectKeys: " ++ show y

orderClause :: PersistEntity val => SelectOpt val -> DB.Field
orderClause o = case o of
                  Asc f  -> fieldName f DB.=: ( 1 :: Int)
                  Desc f -> fieldName f DB.=: (-1 :: Int)
                  _      -> error "orderClause: expected Asc or Desc"


makeQuery :: (PersistEntity record, PersistEntityBackend record ~ MongoBackend) => [Filter record] -> [SelectOpt record] -> DB.Query
makeQuery filts opts =
    (DB.select (filtersToDoc filts) (collectionName $ dummyFromFilts filts)) {
      DB.limit = fromIntegral limit
    , DB.skip  = fromIntegral offset
    , DB.sort  = orders
    }
  where
    (limit, offset, orders') = limitOffsetOrder opts
    orders = map orderClause orders'

filtersToDoc :: (PersistEntity record, PersistEntityBackend record ~ MongoBackend) => [Filter record] -> DB.Document
filtersToDoc filts =
#ifdef DEBUG
  debug $
#endif
    if null filts then [] else multiFilter AndDollar filts

filterToDocument :: (PersistEntity val, PersistEntityBackend val ~ MongoBackend) => Filter val -> DB.Document
filterToDocument f =
    case f of
      Filter field v filt -> [filterToBSON (fieldName field) v filt]
      BackendFilter mf -> mongoFilterToDoc mf
      -- The empty filter case should never occur when the user uses ||.
      -- An empty filter list will throw an exception in multiFilter
      --
      -- The alternative would be to create a query which always returns true
      -- However, I don't think an end user ever wants that.
      FilterOr fs  -> multiFilter OrDollar fs
      -- Ignore an empty filter list instead of throwing an exception.
      -- \$and is necessary in only a few cases, but it makes query construction easier
      FilterAnd [] -> []
      FilterAnd fs -> multiFilter AndDollar fs

data MultiFilter = OrDollar | AndDollar deriving Show
toMultiOp :: MultiFilter -> Text
toMultiOp OrDollar  = orDollar
toMultiOp AndDollar = andDollar

multiFilter :: forall record. (PersistEntity record, PersistEntityBackend record ~ MongoBackend) => MultiFilter -> [Filter record] -> [DB.Field]
multiFilter _ [] = throw $ PersistMongoDBError "An empty list of filters was given"
multiFilter multi filters =
  case (multi, filter (not . null) (map filterToDocument filters)) of
    -- a $or must have at least 2 items
    (OrDollar,  []) -> orError
    (AndDollar, []) -> []
    (OrDollar,    _:[]) -> orError
    (AndDollar, doc:[]) -> doc
    (_, doc) -> [toMultiOp multi DB.:= DB.Array (map DB.Doc doc)]
  where
    orError = throw $ PersistMongoDBError $
        "An empty list of filters was given to one side of ||."

existsDollar, orDollar, andDollar :: Text
existsDollar = "$exists"
orDollar = "$or"
andDollar = "$and"

filterToBSON :: forall a. ( PersistField a)
             => Text
             -> Either a [a]
             -> PersistFilter
             -> DB.Field
filterToBSON fname v filt = case filt of
    Eq -> nullEq
    Ne -> nullNeq
    _  -> notEquality
  where
    dbv = toValue v
    notEquality = fname DB.=: [showFilter filt DB.:= dbv]

    nullEq = case dbv of
      DB.Null -> orDollar DB.=:
        [ [fname DB.:= DB.Null]
        , [fname DB.:= DB.Doc [existsDollar DB.:= DB.Bool False]]
        ]
      _ -> fname DB.:= dbv

    nullNeq = case dbv of
      DB.Null ->
        fname DB.:= DB.Doc
          [ showFilter Ne DB.:= DB.Null
          , existsDollar DB.:= DB.Bool True
          ]
      _ -> notEquality

    showFilter Ne = "$ne"
    showFilter Gt = "$gt"
    showFilter Lt = "$lt"
    showFilter Ge = "$gte"
    showFilter Le = "$lte"
    showFilter In = "$in"
    showFilter NotIn = "$nin"
    showFilter Eq = error "EQ filter not expected"
    showFilter (BackendSpecificFilter bsf) = throw $ PersistMongoDBError $ T.pack $ "did not expect BackendSpecificFilter " ++ T.unpack bsf


mongoFilterToBSON :: forall typ. ( PersistField typ )
                  => Text
                  -> MongoFilterOperator typ
                  -> DB.Document
mongoFilterToBSON fname filt = case filt of
    (PersistOperator v op)     -> [filterToBSON fname v op]
    (MongoFilterOperator bval) -> [fname DB.:= bval]

mongoFilterToDoc :: PersistEntity val => MongoFilter val -> DB.Document
mongoFilterToDoc (RegExpFilter fn (reg, opts))        = [ fieldName fn  DB.:= DB.RegEx (DB.Regex reg opts)]
mongoFilterToDoc (MultiKeyFilter field op) = mongoFilterToBSON (fieldName field) op
mongoFilterToDoc (NestedFilter   field op) = mongoFilterToBSON (nestedFieldName field) op
  where
    nestedFieldName = T.intercalate "." . nesFldName
    nesFldName :: forall r1 r2. (PersistEntity r1) => NestedField r1 r2 -> [DB.Label]
    nesFldName (nf1 `LastEmbFld` nf2)          = [fieldName nf1, fieldName nf2]
    nesFldName ( f1 `MidEmbFld`  f2)           = fieldName f1 : nesFldName f2
    nesFldName ( f1 `MidNestFlds` f2)          = fieldName f1 : nesFldName f2
    nesFldName ( f1 `MidNestFldsNullable` f2)  = fieldName f1 : nesFldName f2
    nesFldName (nf1 `LastNestFld` nf2)         = [fieldName nf1, fieldName nf2]
    nesFldName (nf1 `LastNestFldNullable` nf2) = [fieldName nf1, fieldName nf2]

toValue :: forall a.  PersistField a => Either a [a] -> DB.Value
toValue val =
    case val of
      Left v   -> DB.val $ toPersistValue v
      Right vs -> DB.val $ map toPersistValue vs

fieldName ::  forall record typ.  (PersistEntity record) => EntityField record typ -> DB.Label
fieldName = idfix . unDBName . fieldDB . persistFieldDef
  where idfix f = if f == "id" then _id else f


docToEntityEither :: forall record. (PersistEntity record) => DB.Document -> Either T.Text (Entity record)
docToEntityEither doc = entity
  where
    entDef = entityDef $ Just (getType entity)
    entity = eitherFromPersistValues entDef doc
    getType :: Either err (Entity ent) -> ent
    getType = error "docToEntityEither/getType: never here"

docToEntityThrow :: forall m record. (Trans.MonadIO m, PersistEntity record, PersistEntityBackend record ~ MongoBackend) => DB.Document -> m (Entity record)
docToEntityThrow doc =
    case docToEntityEither doc of
        Left s -> Trans.liftIO . throwIO $ PersistMarshalError $ s
        Right entity -> return entity


fromPersistValuesThrow :: (Trans.MonadIO m, PersistEntity record, PersistEntityBackend record ~ MongoBackend) => EntityDef -> [DB.Field] -> m (Entity record)
fromPersistValuesThrow entDef doc = 
    case eitherFromPersistValues entDef doc of
        Left t -> Trans.liftIO . throwIO $ PersistMarshalError $
                   unHaskellName (entityHaskell entDef) `mappend` ": " `mappend` t
        Right entity -> return entity

eitherFromPersistValues :: (PersistEntity record) => EntityDef -> [DB.Field] -> Either T.Text (Entity record)
eitherFromPersistValues entDef doc =
    let castDoc = assocListFromDoc doc
        -- normally _id is the first field
        mKey = lookup _id castDoc
    in case mKey of
         Nothing -> Left "could not find _id field"
         Just kpv -> fromPersistValues (map snd $ orderPersistValues (toEmbeddedDef entDef) castDoc)
            >>= \body -> keyFromValues [kpv]
            >>= \key   -> Right $ Entity key body

-- | unlike many SQL databases, MongoDB makes no guarantee of the ordering
-- of the fields returned in the document.
-- Ordering might be maintained if persistent were the only user of the db,
-- but other tools may be using MongoDB.
--
-- Persistent creates a Haskell record from a list of PersistValue
-- But most importantly it puts all PersistValues in the proper order
orderPersistValues :: EmbeddedDef -> [(Text, PersistValue)] -> [(Text, PersistValue)]
orderPersistValues entDef castDoc = reorder
  where
    castColumns = map nameAndEmbedded (embeddedFields entDef)
    nameAndEmbedded fdef = (fieldToLabel fdef, emFieldEmbedded fdef)

    -- TODO: the below reasoning should be re-thought now that we are no longer inserting null: searching for a null column will look at every returned field before giving up
    -- Also, we are now doing the _id lookup at the start.
    --
    -- we have an alist of fields that need to be the same order as entityColumns
    --
    -- this naive lookup is O(n^2)
    -- reorder = map (fromJust . (flip Prelude.lookup $ castDoc)) castColumns
    --
    -- this is O(n * log(n))
    -- reorder =  map (\c -> (M.fromList castDoc) M.! c) castColumns 
    --
    -- and finally, this is O(n * log(n))
    -- * do an alist lookup for each column
    -- * but once we found an item in the alist use a new alist without that item for future lookups
    -- * so for the last query there is only one item left
    --
    reorder :: [(Text, PersistValue)] 
    reorder = match castColumns castDoc []
      where
        match :: [(Text, Maybe EmbeddedDef)]
              -> [(Text, PersistValue)]
              -> [(Text, PersistValue)]
              -> [(Text, PersistValue)]
        -- when there are no more Persistent castColumns we are done
        --
        -- allow extra mongoDB fields that persistent does not know about
        -- another application may use fields we don't care about
        -- our own application may set extra fields with the raw driver
        -- TODO: instead use a projection to avoid network overhead
        match [] _ values = values
        match (column:columns) fields values =
          let (found, unused) = matchOne fields []
          in match columns unused $ values ++
                [(fst column, nestedOrder (snd column) (snd found))]
          where
            nestedOrder (Just em) (PersistMap m) =
              PersistMap $ orderPersistValues em m
            nestedOrder (Just em) (PersistList l) =
              PersistList $ map (nestedOrder (Just em)) l
            -- implied: nestedOrder Nothing found = found
            nestedOrder _ found = found

            matchOne (field:fs) tried =
              if fst column == fst field
                -- snd drops the name now that it has been used to make the match
                -- persistent will add the field name later
                then (field, tried ++ fs)
                else matchOne fs (field:tried)
            -- if field is not found, assume it was a Nothing
            --
            -- a Nothing could be stored as null, but that would take up space.
            -- instead, we want to store no field at all: that takes less space.
            -- Also, another ORM may be doing the same
            -- Also, this adding a Maybe field means no migration required
            matchOne [] tried = ((fst column, PersistNull), tried)

assocListFromDoc :: DB.Document -> [(Text, PersistValue)]
assocListFromDoc = Prelude.map (\f -> ( (DB.label f), cast (DB.value f) ) )

oidToPersistValue :: DB.ObjectId -> PersistValue
oidToPersistValue = PersistObjectId . Serialize.encode

oidToKey :: DB.ObjectId -> BackendKey MongoBackend
oidToKey = MongoBackendKey
{-# Deprecated oidToKey "Use MongoBackendKey" #-}

persistObjectIdToDbOid :: PersistValue -> DB.ObjectId
persistObjectIdToDbOid (PersistObjectId k) = case Serialize.decode k of
                  Left msg -> throw $ PersistError $ T.pack $ "error decoding " ++ (show k) ++ ": " ++ msg
                  Right o -> o
persistObjectIdToDbOid _ = throw $ PersistInvalidField "expected PersistObjectId"

keyToOid :: (PersistEntity entity, PersistEntityBackend entity ~ MongoBackend)
         => BackendKey MongoBackend -> DB.ObjectId
keyToOid = unMongoBackendKey
{-# Deprecated keyToOid "Use unMongoBackendKey" #-}

instance DB.Val PersistValue where
  val (PersistInt64 x)   = DB.Int64 x
  val (PersistText x)    = DB.String x
  val (PersistDouble x)  = DB.Float x
  val (PersistBool x)    = DB.Bool x
#ifdef HIGH_PRECISION_DATE
  val (PersistUTCTime x) = DB.Int64 $ round $ 1000 * 1000 * 1000 * (utcTimeToPOSIXSeconds x)
#else
  -- this is just millisecond precision: https://jira.mongodb.org/browse/SERVER-1460
  val (PersistUTCTime x) = DB.UTC x
#endif
  val (PersistDay d)     = DB.Int64 $ fromInteger $ toModifiedJulianDay d
  val (PersistNull)      = DB.Null
  val (PersistList l)    = DB.Array $ map DB.val l
  val (PersistMap  m)    = DB.Doc $ map (\(k, v)-> (DB.=:) k v) m
  val (PersistByteString x) = DB.Bin (DB.Binary x)
  val x@(PersistObjectId _) = DB.ObjId $ persistObjectIdToDbOid x
  val (PersistTimeOfDay _)  = throw $ PersistMongoDBUnsupported "PersistTimeOfDay not implemented for the MongoDB backend. only PersistUTCTime currently implemented"
  val (PersistRational _)   = throw $ PersistMongoDBUnsupported "PersistRational not implemented for the MongoDB backend"
  val (PersistDbSpecific _)   = throw $ PersistMongoDBUnsupported "PersistDbSpecific not implemented for the MongoDB backend"
  cast' (DB.Float x)  = Just (PersistDouble x)
  cast' (DB.Int32 x)  = Just $ PersistInt64 $ fromIntegral x
  cast' (DB.Int64 x)  = Just $ PersistInt64 x
  cast' (DB.String x) = Just $ PersistText x
  cast' (DB.Bool x)   = Just $ PersistBool x
  cast' (DB.UTC d)    = Just $ PersistUTCTime d
  cast' DB.Null       = Just $ PersistNull
  cast' (DB.Bin (DB.Binary b))   = Just $ PersistByteString b
  cast' (DB.Fun (DB.Function f)) = Just $ PersistByteString f
  cast' (DB.Uuid (DB.UUID uid))  = Just $ PersistByteString uid
  cast' (DB.Md5 (DB.MD5 md5))    = Just $ PersistByteString md5
  cast' (DB.UserDef (DB.UserDefined bs)) = Just $ PersistByteString bs
  cast' (DB.RegEx (DB.Regex us1 us2))    = Just $ PersistByteString $ E.encodeUtf8 $ T.append us1 us2
  cast' (DB.Doc doc)  = Just $ PersistMap $ assocListFromDoc doc
  cast' (DB.Array xs) = Just $ PersistList $ mapMaybe DB.cast' xs
  cast' (DB.ObjId x)  = Just $ oidToPersistValue x 
  cast' (DB.JavaScr _) = throw $ PersistMongoDBUnsupported "cast operation not supported for javascript"
  cast' (DB.Sym _)     = throw $ PersistMongoDBUnsupported "cast operation not supported for sym"
  cast' (DB.Stamp _)   = throw $ PersistMongoDBUnsupported "cast operation not supported for stamp"
  cast' (DB.MinMax _)  = throw $ PersistMongoDBUnsupported "cast operation not supported for minmax"

cast :: DB.Value -> PersistValue
-- since we have case analysys this won't ever be Nothing
-- However, unsupported types do throw an exception in pure code
-- probably should re-work this to throw in IO
cast = fromJust . DB.cast'

instance Serialize.Serialize DB.ObjectId where
  put (DB.Oid w1 w2) = do Serialize.put w1
                          Serialize.put w2

  get = do w1 <- Serialize.get
           w2 <- Serialize.get
           return (DB.Oid w1 w2) 

dummyFromUnique :: Unique v -> v
dummyFromUnique _ = error "dummyFromUnique"
dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"

data MongoAuth = MongoAuth DB.Username DB.Password deriving Show

-- | Information required to connect to a mongo database
data MongoConf = MongoConf
    { mgDatabase :: Text
    , mgHost     :: Text
    , mgPort     :: PortID
    , mgAuth     :: Maybe MongoAuth
    , mgAccessMode :: DB.AccessMode
    , mgPoolStripes :: Int
    , mgStripeConnections :: Int
    , mgConnectionIdleTime :: NominalDiffTime
    -- | YAML fields for this are @rsName@ and @rsSecondaries@
    -- mgHost is assumed to be the primary
    , mgReplicaSetConfig :: Maybe ReplicaSetConfig
    } deriving Show

defaultHost :: Text
defaultHost = "127.0.0.1"
defaultAccessMode :: DB.AccessMode
defaultAccessMode = DB.ConfirmWrites ["j" DB.=: True]
defaultPoolStripes, defaultStripeConnections :: Int
defaultPoolStripes = 1
defaultStripeConnections = 10
defaultConnectionIdleTime :: NominalDiffTime
defaultConnectionIdleTime = 20

defaultMongoConf :: Text -> MongoConf
defaultMongoConf dbName = MongoConf
  { mgDatabase = dbName
  , mgHost = defaultHost
  , mgPort = DB.defaultPort
  , mgAuth = Nothing
  , mgAccessMode = defaultAccessMode
  , mgPoolStripes = defaultPoolStripes
  , mgStripeConnections = defaultStripeConnections
  , mgConnectionIdleTime = defaultConnectionIdleTime
  , mgReplicaSetConfig = Nothing
  }

data ReplicaSetConfig = ReplicaSetConfig DB.ReplicaSetName [DB.Host]
    deriving Show


instance PersistConfig MongoConf where
    type PersistConfigBackend MongoConf = DB.Action
    type PersistConfigPool MongoConf = ConnectionPool

    createPoolConfig = createMongoPool

    runPool c = runMongoDBPool (mgAccessMode c)
    loadConfig (Object o) = do
        db                  <- o .:  "database"
        host                <- o .:? "host" .!= defaultHost
        NoOrphanPortID port <- o .:? "port" .!= NoOrphanPortID DB.defaultPort
        poolStripes         <- o .:? "poolstripes" .!= defaultPoolStripes
        stripeConnections   <- o .:? "connections" .!= defaultStripeConnections
        NoOrphanNominalDiffTime connectionIdleTime <- o .:? "connectionIdleTime" .!= NoOrphanNominalDiffTime defaultConnectionIdleTime
        mUser              <- o .:? "user"
        mPass              <- o .:? "password"
        accessString       <- o .:? "accessMode" .!= confirmWrites
        mRsName            <- o .:? "rsName"
        rsSecondaires      <- o .:? "rsSecondaries" .!= []

        mPoolSize         <- o .:? "poolsize"
        case mPoolSize of
          Nothing -> return ()
          Just (_::Int) -> fail "specified deprecated poolsize attribute. Please specify a connections. You can also specify a pools attribute which defaults to 1. Total connections opened to the db are connections * pools"

        accessMode <- case accessString of
             "ReadStaleOk"       -> return DB.ReadStaleOk
             "UnconfirmedWrites" -> return DB.UnconfirmedWrites
             "ConfirmWrites"     -> return defaultAccessMode
             badAccess -> fail $ "unknown accessMode: " ++ T.unpack badAccess

        let rs = case (mRsName, rsSecondaires) of
                     (Nothing, []) -> Nothing
                     (Nothing, _) -> error "found rsSecondaries key. Also expected but did not find a rsName key"
                     (Just rsName, hosts) -> Just $ ReplicaSetConfig rsName $ fmap DB.readHostPort hosts

        return MongoConf {
            mgDatabase = db
          , mgHost = host
          , mgPort = port
          , mgAuth =
              case (mUser, mPass) of
                (Just user, Just pass) -> Just (MongoAuth user pass)
                _ -> Nothing
          , mgPoolStripes = poolStripes
          , mgStripeConnections = stripeConnections
          , mgAccessMode = accessMode
          , mgConnectionIdleTime = connectionIdleTime
          , mgReplicaSetConfig = rs
          }
      where
        confirmWrites = "ConfirmWrites"
    {-
        safeRead :: String -> T.Text -> MEither String Int
        safeRead name t = case reads s of
            (i, _):_ -> MRight i
            []       -> MLeft $ concat ["Invalid value for ", name, ": ", s]
          where
            s = T.unpack t
            -}
    loadConfig _ = mzero

-- | docker integration: change the host to the mongodb link
applyDockerEnv :: MongoConf -> IO MongoConf
applyDockerEnv mconf = do
    mHost <- lookupEnv "MONGODB_PORT_27017_TCP_ADDR"
    return $ case mHost of
        Nothing -> mconf
        Just h -> mconf { mgHost = T.pack h }


-- ---------------------------
-- * MongoDB specific Filters

-- $filters
--
-- You can find example usage for all of Persistent in our test cases:
-- <https://github.com/yesodweb/persistent/blob/master/persistent-test/EmbedTest.hs#L144>
--
-- These filters create a query that reaches deeper into a document with
-- nested fields.

type instance BackendSpecificFilter MongoBackend record = MongoFilter record

data NestedField record typ
  = forall emb. PersistEntity emb => EntityField record [emb] `LastEmbFld` EntityField emb typ
  | forall emb. PersistEntity emb => EntityField record [emb] `MidEmbFld` NestedField emb typ
  | forall nest. PersistEntity nest => EntityField record nest  `MidNestFlds` NestedField nest typ
  | forall nest. PersistEntity nest => EntityField record (Maybe nest) `MidNestFldsNullable` NestedField nest typ
  | forall nest. PersistEntity nest => EntityField record nest `LastNestFld` EntityField nest typ
  | forall nest. PersistEntity nest => EntityField record (Maybe nest) `LastNestFldNullable` EntityField nest typ

-- | A MongoRegex represetns a Regular expression.
-- It is a tuple of the expression and the options for the regular expression, respectively
-- Options are listed here: <http://docs.mongodb.org/manual/reference/operator/query/regex/>
-- If you use the same options you may want to define a helper such as @r t = (t, "ims")@
type MongoRegex = (Text, Text)

-- | Mark the subset of 'PersistField's that can be searched by a mongoDB regex
-- Anything stored as PersistText or an array of PersistText would be valid
class PersistField typ => MongoRegexSearchable typ where

instance MongoRegexSearchable Text
instance MongoRegexSearchable rs => MongoRegexSearchable (Maybe rs)
instance MongoRegexSearchable rs => MongoRegexSearchable [rs]

-- | Filter using a Regular expression.
(=~.) :: forall record searchable. (MongoRegexSearchable searchable, PersistEntity record, PersistEntityBackend record ~ MongoBackend) => EntityField record searchable -> MongoRegex -> Filter record
fld =~. val = BackendFilter $ RegExpFilter fld val

-- | Filter using a Regular expression against a nullable field.
(?=~.) :: forall record. (PersistEntity record, PersistEntityBackend record ~ MongoBackend) => EntityField record (Maybe Text) -> MongoRegex -> Filter record
fld ?=~. val = BackendFilter $ RegExpFilter fld val
{-# DEPRECATED (?=~.) "Use =~. instead" #-}

data MongoFilterOperator typ = PersistOperator (Either typ [typ]) PersistFilter
                             | MongoFilterOperator DB.Value

data MongoFilter record = forall typ. (PersistField typ) =>
                        NestedFilter {
                          nestedField  :: NestedField record typ
                        , nestedValue  :: MongoFilterOperator typ
                        }
                      | forall typ. PersistField typ =>
                        MultiKeyFilter {
                          multiField  :: EntityField record [typ]
                        , multiValue  :: MongoFilterOperator typ
                        }
                      | forall typ. MongoRegexSearchable typ =>
                        RegExpFilter (EntityField record typ) MongoRegex

-- | Point to an array field with an embedded object and give a deeper query into the embedded object.
-- Use with 'nestEq'.
(->.) :: forall record emb typ. PersistEntity emb => EntityField record [emb] -> EntityField emb typ -> NestedField record typ
(->.)  = LastEmbFld

-- | Point to an array field with an embedded object and give a deeper query into the embedded object.
-- This level of nesting is not the final level.
-- Use '->.' or '&->.' to point to the final level.
(~>.) :: forall record typ emb. PersistEntity emb => EntityField record [emb] -> NestedField emb typ -> NestedField record typ
(~>.)  = MidEmbFld

-- | Point to a nested field to query. This field is not an array type.
-- Use with 'nestEq'.
(&->.) :: forall record typ nest. PersistEntity nest => EntityField record nest -> EntityField nest typ -> NestedField record typ
(&->.) = LastNestFld

-- | Same as '&->.', but Works against a Maybe type
(?&->.) :: forall record typ nest. PersistEntity nest => EntityField record (Maybe nest) -> EntityField nest typ -> NestedField record typ
(?&->.) = LastNestFldNullable


-- | Point to a nested field to query. This field is not an array type.
-- This level of nesting is not the final level.
-- Use '->.' or '&>.' to point to the final level.
(&~>.) :: forall val nes nes1. PersistEntity nes1 => EntityField val nes1 -> NestedField nes1 nes -> NestedField val nes
(&~>.)  = MidNestFlds

-- | Same as '&~>.', but works against a Maybe type
(?&~>.) :: forall val nes nes1. PersistEntity nes1 => EntityField val (Maybe nes1) -> NestedField nes1 nes -> NestedField val nes
(?&~>.) = MidNestFldsNullable


infixr 4 ?=~.
infixr 4 =~.
infixr 5 ~>.
infixr 5 &~>.
infixr 5 ?&~>.
infixr 6 &->.
infixr 6 ?&->.
infixr 6 ->.

infixr 4 `nestEq`
infixr 4 `nestNe`
infixr 4 `nestGe`
infixr 4 `nestLe`
infixr 4 `nestIn`
infixr 4 `nestNotIn`

infixr 4 `anyEq`
infixr 4 `multiEq`
infixr 4 `nestBsonEq`
infixr 4 `multiBsonEq`
infixr 4 `anyBsonEq`

-- | The normal Persistent equality test '==.' is not generic enough.
-- Instead use this with the drill-down arrow operaters such as '->.'
--
-- using this as the only query filter is similar to the following in the mongoDB shell
--
-- > db.Collection.find({"object.field": item})
nestEq, nestNe, nestGe, nestLe, nestIn, nestNotIn :: forall record typ.
    ( PersistField typ , PersistEntityBackend record ~ MongoBackend)
    => NestedField record typ
    -> typ
    -> Filter record
nestEq = nestedOp Eq
nestNe = nestedOp Ne
nestGe = nestedOp Ge
nestLe = nestedOp Le
nestIn = nestedOp In
nestNotIn = nestedOp NotIn

nestedOp :: forall record typ.
       ( PersistField typ
       , PersistEntityBackend record ~ MongoBackend
       ) => PersistFilter -> NestedField record typ -> typ -> Filter record
nestedOp op nf v = BackendFilter $ NestedFilter
                    { nestedField = nf
                    , nestedValue = PersistOperator (Left v) op
                    }

-- | same as `nestEq`, but give a BSON Value
nestBsonEq :: forall record typ.
       ( PersistField typ
       , PersistEntityBackend record ~ MongoBackend
       ) => NestedField record typ -> DB.Value -> Filter record
nf `nestBsonEq` val = BackendFilter $ NestedFilter
                    { nestedField = nf
                    , nestedValue = MongoFilterOperator val
                    }

multiEq :: forall record typ.
        ( PersistField typ
        , PersistEntityBackend record ~ MongoBackend
        ) => EntityField record [typ] -> typ -> Filter record
multiEq = anyEq
{-# DEPRECATED multiEq "Please use anyEq instead" #-}

-- | Like nestEq, but for an embedded list.
-- Checks to see if the list contains an item.
--
-- In Haskell we need different equality functions for embedded fields that are lists or non-lists to keep things type-safe.
--
-- using this as the only query filter is similar to the following in the mongoDB shell
--
-- > db.Collection.find({arrayField: arrayItem})
anyEq :: forall record typ.
        ( PersistField typ
        , PersistEntityBackend record ~ MongoBackend
        ) => EntityField record [typ] -> typ -> Filter record
fld `anyEq` val = BackendFilter $ MultiKeyFilter
                      { multiField = fld
                      , multiValue = PersistOperator (Left val) Eq
                      }

multiBsonEq :: forall record typ.
        ( PersistField typ
        , PersistEntityBackend record ~ MongoBackend
        ) => EntityField record [typ] -> DB.Value -> Filter record
multiBsonEq = anyBsonEq
{-# DEPRECATED multiBsonEq "Please use anyBsonEq instead" #-}

-- | same as `anyEq`, but give a BSON Value
anyBsonEq :: forall record typ.
        ( PersistField typ
        , PersistEntityBackend record ~ MongoBackend
        ) => EntityField record [typ] -> DB.Value -> Filter record
fld `anyBsonEq` val = BackendFilter $ MultiKeyFilter
                      { multiField = fld
                      , multiValue = MongoFilterOperator val
                      }

