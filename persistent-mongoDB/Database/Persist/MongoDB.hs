{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
module Database.Persist.MongoDB
    (
    -- * Entity conversion
      collectionName
    , docToEntityEither
    , docToEntityThrow
    , recordToDocument
    , documentFromEntity
    , toInsertDoc
    , entityToInsertDoc
    , updatesToDoc
    , filtersToDoc
    , toUniquesDoc

    -- * MongoDB specific queries
    -- $nested
    , (->.), (~>.), (?&->.), (?&~>.), (&->.), (&~>.)
    -- ** Filters
    -- $filters
    , nestEq, nestNe, nestGe, nestLe, nestIn, nestNotIn
    , anyEq, nestAnyEq, nestBsonEq, anyBsonEq
    , inList, ninList
    , (=~.)
    -- non-operator forms of filters
    , NestedField(..)
    , MongoRegexSearchable
    , MongoRegex

    -- ** Updates
    -- $updates
    , nestSet, nestInc, nestDec, nestMul, push, pull, pullAll, addToSet, eachOp

    -- * Key conversion helpers
    , BackendKey(..)
    , keyToOid
    , oidToKey
    , recordTypeFromKey
    , readMayObjectId
    , readMayMongoKey
    , keyToText

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

    -- * MongoDB driver types
    , Database
    , DB.Action
    , DB.AccessMode(..)
    , DB.master
    , DB.slaveOk
    , (DB.=:)
    , DB.ObjectId
    , DB.MongoContext
    , DB.PortID

    -- * Database.Persist
    , module Database.Persist
    ) where

import Control.Exception (throw, throwIO)
import Control.Monad (liftM, (>=>), forM_, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.IO.Class as Trans
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Trans.Reader (ask, runReaderT)

import Data.Acquire (mkAcquire)
import Data.Aeson (Value (Number), (.:), (.:?), (.!=), FromJSON(..), ToJSON(..), withText, withObject)
import Data.Aeson.Types (modifyFailure)
import Data.Bits (shiftR)
import Data.Bson (ObjectId(..))
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Maybe (mapMaybe, fromJust)
import Data.Monoid (mappend)
import qualified Data.Serialize as Serialize
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Traversable as Traversable
import qualified Data.Pool as Pool
import Data.Time (NominalDiffTime)
import Data.Time.Calendar (Day(..))
#ifdef HIGH_PRECISION_DATE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
#endif
import Data.Word (Word16)
import Network.Socket (HostName)
import Numeric (readHex)
import System.Environment (lookupEnv)
import Unsafe.Coerce (unsafeCoerce)
import Web.PathPieces (PathPiece(..))
import Web.HttpApiData (ToHttpApiData(..), FromHttpApiData(..), parseUrlPieceMaybe, parseUrlPieceWithPrefix, readTextData)

#ifdef DEBUG
import FileLocation (debug)
#endif

import qualified Database.MongoDB as DB
import Database.MongoDB.Query (Database)

import Database.Persist
import qualified Database.Persist.Sql as Sql

instance HasPersistBackend DB.MongoContext where
    type BaseBackend DB.MongoContext = DB.MongoContext
    persistBackend = id

recordTypeFromKey :: Key record -> record
recordTypeFromKey _ = error "recordTypeFromKey"

newtype NoOrphanNominalDiffTime = NoOrphanNominalDiffTime NominalDiffTime
                                deriving (Show, Eq, Num)

instance FromJSON NoOrphanNominalDiffTime where
    parseJSON (Number x) = (return . NoOrphanNominalDiffTime . fromRational . toRational) x
    parseJSON _ = fail "couldn't parse diff time"

newtype NoOrphanPortID = NoOrphanPortID DB.PortID deriving (Show, Eq)


instance FromJSON NoOrphanPortID where
    parseJSON (Number  x) = (return . NoOrphanPortID . DB.PortNumber . fromIntegral ) cnvX
      where cnvX :: Word16
            cnvX = round x
    parseJSON _ = fail "couldn't parse port number"


data Connection = Connection DB.Pipe DB.Database
type ConnectionPool = Pool.Pool Connection

instance ToHttpApiData (BackendKey DB.MongoContext) where
    toUrlPiece = keyToText

instance FromHttpApiData (BackendKey DB.MongoContext) where
    parseUrlPiece input = do
      s <- parseUrlPieceWithPrefix "o" input <!> return input
      MongoKey <$> readTextData s
      where
        infixl 3 <!>
        Left _ <!> y = y
        x      <!> _ = x

-- | ToPathPiece is used to convert a key to/from text
instance PathPiece (BackendKey DB.MongoContext) where
  toPathPiece   = toUrlPiece
  fromPathPiece = parseUrlPieceMaybe

keyToText :: BackendKey DB.MongoContext -> Text
keyToText = T.pack . show . unMongoKey

-- | Convert a Text to a Key
readMayMongoKey :: Text -> Maybe (BackendKey DB.MongoContext)
readMayMongoKey = fmap MongoKey . readMayObjectId

readMayObjectId :: Text -> Maybe DB.ObjectId
readMayObjectId str =
  case filter (null . snd) $ reads $ T.unpack str :: [(DB.ObjectId,String)] of
    (parsed,_):[] -> Just parsed
    _ -> Nothing

instance PersistField DB.ObjectId where
    toPersistValue = oidToPersistValue
    fromPersistValue oid@(PersistObjectId _) = Right $ persistObjectIdToDbOid oid
    fromPersistValue (PersistByteString bs) = fromPersistValue (PersistObjectId bs)
    fromPersistValue _ = Left $ T.pack "expected PersistObjectId"

instance Sql.PersistFieldSql DB.ObjectId where
    sqlType _ = Sql.SqlOther "doesn't make much sense for MongoDB"

instance Sql.PersistFieldSql (BackendKey DB.MongoContext) where
    sqlType _ = Sql.SqlOther "doesn't make much sense for MongoDB"


withConnection :: (Trans.MonadIO m)
               => MongoConf
               -> (ConnectionPool -> m b) -> m b
withConnection mc =
  withMongoDBPool (mgDatabase mc) (T.unpack $ mgHost mc) (mgPort mc) (mgAuth mc) (mgPoolStripes mc) (mgStripeConnections mc) (mgConnectionIdleTime mc)

withMongoDBConn :: (Trans.MonadIO m)
                => Database -> HostName -> DB.PortID
                -> Maybe MongoAuth -> NominalDiffTime
                -> (ConnectionPool -> m b) -> m b
withMongoDBConn dbname hostname port mauth connectionIdleTime = withMongoDBPool dbname hostname port mauth 1 1 connectionIdleTime

createPipe :: HostName -> DB.PortID -> IO DB.Pipe
createPipe hostname port = DB.connect (DB.Host hostname port)

createReplicatSet :: (DB.ReplicaSetName, [DB.Host]) -> Database -> Maybe MongoAuth -> IO Connection
createReplicatSet rsSeed dbname mAuth = do
    pipe <- DB.openReplicaSet rsSeed >>= DB.primary
    testAccess pipe dbname mAuth
    return $ Connection pipe dbname

createRsPool :: (Trans.MonadIO m) => Database -> ReplicaSetConfig
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

createConnection :: Database -> HostName -> DB.PortID -> Maybe MongoAuth -> IO Connection
createConnection dbname hostname port mAuth = do
    pipe <- createPipe hostname port
    testAccess pipe dbname mAuth
    return $ Connection pipe dbname

createMongoDBPool :: (Trans.MonadIO m) => Database -> HostName -> DB.PortID
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


createMongoPool :: (Trans.MonadIO m) => MongoConf -> m ConnectionPool
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
createMongoDBPipePool :: (Trans.MonadIO m) => HostName -> DB.PortID
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

withMongoPool :: (Trans.MonadIO m) => MongoConf -> (ConnectionPool -> m b) -> m b
withMongoPool conf connectionReader = createMongoPool conf >>= connectionReader

withMongoDBPool :: (Trans.MonadIO m) =>
  Database -> HostName -> DB.PortID -> Maybe MongoAuth -> Int -> Int -> NominalDiffTime -> (ConnectionPool -> m b) -> m b
withMongoDBPool dbname hostname port mauth poolStripes stripeConnections connectionIdleTime connectionReader = do
  pool <- createMongoDBPool dbname hostname port mauth poolStripes stripeConnections connectionIdleTime
  connectionReader pool

-- | run a pool created with 'createMongoDBPipePool'
runMongoDBPipePool :: MonadUnliftIO m => DB.AccessMode -> Database -> DB.Action m a -> PipePool -> m a
runMongoDBPipePool accessMode db action pool =
  withRunInIO $ \run ->
  Pool.withResource pool $ \pipe ->
  run $ DB.access pipe accessMode db action

runMongoDBPool :: MonadUnliftIO m => DB.AccessMode  -> DB.Action m a -> ConnectionPool -> m a
runMongoDBPool accessMode action pool =
  withRunInIO $ \run ->
  Pool.withResource pool $ \(Connection pipe db) ->
  run $ DB.access pipe accessMode db action


-- | use default 'AccessMode'
runMongoDBPoolDef :: MonadUnliftIO m => DB.Action m a -> ConnectionPool -> m a
runMongoDBPoolDef = runMongoDBPool defaultAccessMode

queryByKey :: (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext)
           => Key record -> DB.Query
queryByKey k = (DB.select (keyToMongoDoc k) (collectionNameFromKey k)) {DB.project = projectionFromKey k}

selectByKey :: (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext)
            => Key record -> DB.Selection
selectByKey k = DB.select (keyToMongoDoc k) (collectionNameFromKey k)

updatesToDoc :: (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext)
             => [Update record] -> DB.Document
updatesToDoc upds = map updateToMongoField upds

updateToBson :: Text
             -> PersistValue
             -> Either PersistUpdate MongoUpdateOperation
             -> DB.Field
updateToBson fname v up =
#ifdef DEBUG
  debug (
#endif
    opName DB.:= DB.Doc [fname DB.:= opValue]
#ifdef DEBUG
    )
#endif
  where
    inc = "$inc"
    mul = "$mul"
    (opName, opValue) = case up of
      Left pup -> case (pup, v) of
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
        (BackendSpecificUpdate bsup, _) -> throw $ PersistMongoDBError $
          T.pack $ "did not expect BackendSpecificUpdate " ++ T.unpack bsup
      Right mup -> case mup of
        MongoEach op  -> case op of
           MongoPull -> ("$pullAll", DB.val v)
           _         -> (opToText op, DB.Doc ["$each" DB.:= DB.val v])
        MongoSimple x -> (opToText x, DB.val v)




updateToMongoField :: (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext)
                   => Update record -> DB.Field
updateToMongoField (Update field v up) = updateToBson (fieldName field) (toPersistValue v) (Left up)
updateToMongoField (BackendUpdate up)  = mongoUpdateToDoc up


-- | convert a unique key into a MongoDB document
toUniquesDoc :: forall record. (PersistEntity record) => Unique record -> [DB.Field]
toUniquesDoc uniq = zipWith (DB.:=)
  (map (unDBName . snd) $ persistUniqueToFieldNames uniq)
  (map DB.val (persistUniqueToValues uniq))

-- | convert a PersistEntity into document fields.
-- for inserts only: nulls are ignored so they will be unset in the document.
-- 'recordToDocument' includes nulls
toInsertDoc :: forall record.  (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext)
            => record -> DB.Document
toInsertDoc record = zipFilter (embeddedFields $ toEmbedEntityDef entDef)
    (map toPersistValue $ toPersistFields record)
  where
    entDef = entityDef $ Just record
    zipFilter :: [EmbedFieldDef] -> [PersistValue] -> DB.Document
    zipFilter [] _  = []
    zipFilter _  [] = []
    zipFilter (fd:efields) (pv:pvs) =
        if isNull pv then recur else
          (fieldToLabel fd DB.:= embeddedVal (emFieldEmbed fd) pv):recur

      where
        recur = zipFilter efields pvs

        isNull PersistNull = True
        isNull (PersistMap m) = null m
        isNull (PersistList l) = null l
        isNull _ = False

    -- make sure to removed nulls from embedded entities also
    embeddedVal :: Maybe EmbedEntityDef -> PersistValue -> DB.Value
    embeddedVal (Just emDef) (PersistMap m) = DB.Doc $
      zipFilter (embeddedFields emDef) $ map snd m
    embeddedVal je@(Just _) (PersistList l) = DB.Array $ map (embeddedVal je) l
    embeddedVal _ pv = DB.val pv

entityToInsertDoc :: forall record.  (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext)
                  => Entity record -> DB.Document
entityToInsertDoc (Entity key record) = keyToMongoDoc key ++ toInsertDoc record

collectionName :: (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext)
               => record -> Text
collectionName = unDBName . entityDB . entityDef . Just

-- | convert a PersistEntity into document fields.
-- unlike 'toInsertDoc', nulls are included.
recordToDocument :: (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext)
                 => record -> DB.Document
recordToDocument record = zipToDoc (map fieldDB $ entityFields entity) (toPersistFields record)
  where
    entity = entityDef $ Just record

documentFromEntity :: (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext)
                   => Entity record -> DB.Document
documentFromEntity (Entity key record) =
    keyToMongoDoc key ++ recordToDocument record

zipToDoc :: PersistField a => [DBName] -> [a] -> [DB.Field]
zipToDoc [] _  = []
zipToDoc _  [] = []
zipToDoc (e:efields) (p:pfields) =
  let pv = toPersistValue p
  in  (unDBName e DB.:= DB.val pv):zipToDoc efields pfields

fieldToLabel :: EmbedFieldDef -> Text
fieldToLabel = unDBName . emFieldDB

keyFrom_idEx :: (Trans.MonadIO m, PersistEntity record) => DB.Value -> m (Key record)
keyFrom_idEx idVal = case keyFrom_id idVal of
    Right k  -> return k
    Left err -> liftIO $ throwIO $ PersistMongoDBError $ "could not convert key: "
        `Data.Monoid.mappend` T.pack (show idVal)
        `mappend` err

keyFrom_id :: (PersistEntity record) => DB.Value -> Either Text (Key record)
keyFrom_id idVal = case cast idVal of
    (PersistMap m) -> keyFromValues $ map snd m
    pv -> keyFromValues [pv]

-- | It would make sense to define the instance for ObjectId
-- and then use newtype deriving
-- however, that would create an orphan instance
instance ToJSON (BackendKey DB.MongoContext) where
    toJSON (MongoKey (Oid x y)) = toJSON $ DB.showHexLen 8 x $ DB.showHexLen 16 y ""

instance FromJSON (BackendKey DB.MongoContext) where
    parseJSON = withText "MongoKey" $ \t ->
        maybe
          (fail "Invalid base64")
          (return . MongoKey . persistObjectIdToDbOid . PersistObjectId)
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

-- | older versions versions of haddock (like that on hackage) do not show that this defines
-- @BackendKey DB.MongoContext = MongoKey { unMongoKey :: DB.ObjectId }@
instance PersistCore DB.MongoContext where
    newtype BackendKey DB.MongoContext = MongoKey { unMongoKey :: DB.ObjectId }
        deriving (Show, Read, Eq, Ord, PersistField)

instance PersistStoreWrite DB.MongoContext where
    insert record = DB.insert (collectionName record) (toInsertDoc record)
                >>= keyFrom_idEx

    insertMany [] = return []
    insertMany records@(r:_) = mapM keyFrom_idEx =<<
        DB.insertMany (collectionName r) (map toInsertDoc records)

    insertEntityMany [] = return ()
    insertEntityMany ents@(Entity _ r : _) =
        DB.insertMany_ (collectionName r) (map entityToInsertDoc ents)

    insertKey k record = DB.insert_ (collectionName record) $
                         entityToInsertDoc (Entity k record)

    repsert   k record = DB.save (collectionName record) $
                         documentFromEntity (Entity k record)

    replace k record = do
        DB.replace (selectByKey k) (recordToDocument record)
        return ()

    delete k =
        DB.deleteOne DB.Select {
          DB.coll = collectionNameFromKey k
        , DB.selector = keyToMongoDoc k
        }

    update _ [] = return ()
    update key upds =
        DB.modify
           (DB.Select (keyToMongoDoc key) (collectionNameFromKey key))
           $ updatesToDoc upds

    updateGet key upds = do
        context <- ask
        result <- liftIO $ runReaderT (DB.findAndModify (queryByKey key) (updatesToDoc upds)) context
        either err instantiate result
      where
        instantiate doc = do
            Entity _ rec <- fromPersistValuesThrow t doc
            return rec
        err msg = Trans.liftIO $ throwIO $ KeyNotFound $ show key ++ msg
        t = entityDefFromKey key

instance PersistStoreRead DB.MongoContext where
    get k = do
            d <- DB.findOne (queryByKey k)
            case d of
              Nothing -> return Nothing
              Just doc -> do
                Entity _ ent <- fromPersistValuesThrow t doc
                return $ Just ent
          where
            t = entityDefFromKey k

instance PersistUniqueRead DB.MongoContext where
    getBy uniq = do
        mdoc <- DB.findOne $
          (DB.select (toUniquesDoc uniq) (collectionName rec)) {DB.project = projectionFromRecord rec}
        case mdoc of
            Nothing -> return Nothing
            Just doc -> liftM Just $ fromPersistValuesThrow t doc
      where
        t = entityDef $ Just rec
        rec = dummyFromUnique uniq

instance PersistUniqueWrite DB.MongoContext where
    deleteBy uniq =
        DB.delete DB.Select {
          DB.coll = collectionName $ dummyFromUnique uniq
        , DB.selector = toUniquesDoc uniq
        }

    upsert newRecord upds = do
        uniq <- onlyUnique newRecord
        upsertBy uniq newRecord upds

-- -        let uniqKeys = map DB.label uniqueDoc
-- -        let insDoc = DB.exclude uniqKeys $ toInsertDoc newRecord
--          let selection = DB.select uniqueDoc $ collectionName newRecord
-- -        if null upds
-- -          then DB.upsert selection ["$set" DB.=: insDoc]
-- -          else do
-- -            DB.upsert selection ["$setOnInsert" DB.=: insDoc]
-- -            DB.modify selection $ updatesToDoc upds
-- -        -- because findAndModify $setOnInsert is broken we do a separate get now

    upsertBy uniq newRecord upds = do
        let uniqueDoc = toUniquesDoc uniq :: [DB.Field]
        let uniqKeys = map DB.label uniqueDoc :: [DB.Label]
        let insDoc = DB.exclude uniqKeys $ toInsertDoc newRecord :: DB.Document
        let selection = DB.select uniqueDoc $ collectionName newRecord :: DB.Selection
        mdoc <- getBy uniq
        case mdoc of
          Nothing -> unless (null upds) (DB.upsert selection ["$setOnInsert" DB.=: insDoc])
          Just _ -> unless (null upds) (DB.modify selection $ DB.exclude uniqKeys $ updatesToDoc upds)
        newMdoc <- getBy uniq
        case newMdoc of
          Nothing -> err "possible race condition: getBy found Nothing"
          Just doc -> return doc
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


-- | It would make more sense to call this _id, but GHC treats leading underscore in special ways
id_ :: T.Text
id_ = "_id"

-- _id is always the primary key in MongoDB
-- but _id can contain any unique value
keyToMongoDoc :: (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext)
                  => Key record -> DB.Document
keyToMongoDoc k = case entityPrimary $ entityDefFromKey k of
    Nothing   -> zipToDoc [DBName id_] values
    Just pdef -> [id_ DB.=: zipToDoc (primaryNames pdef)  values]
  where
    primaryNames = map fieldDB . compositeFields
    values = keyToValues k

entityDefFromKey :: PersistEntity record => Key record -> EntityDef
entityDefFromKey = entityDef . Just . recordTypeFromKey

collectionNameFromKey :: (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext)
                      => Key record -> Text
collectionNameFromKey = collectionName . recordTypeFromKey

projectionFromEntityDef :: EntityDef -> DB.Projector
projectionFromEntityDef eDef =
  map toField (entityFields eDef)
  where
    toField :: FieldDef -> DB.Field
    toField fDef = (unDBName (fieldDB fDef)) DB.=: (1 :: Int)

projectionFromKey :: PersistEntity record => Key record -> DB.Projector
projectionFromKey = projectionFromEntityDef . entityDefFromKey

projectionFromRecord :: PersistEntity record => record -> DB.Projector
projectionFromRecord = projectionFromEntityDef . entityDef . Just


instance PersistQueryWrite DB.MongoContext where
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

instance PersistQueryRead DB.MongoContext where
    count filts = do
        i <- DB.count query
        return $ fromIntegral i
      where
        query = DB.select (filtersToDoc filts) $
                  collectionName $ dummyFromFilts filts

    -- | uses cursor option NoCursorTimeout
    -- If there is no sorting, it will turn the $snapshot option on
    -- and explicitly closes the cursor when done
    selectSourceRes filts opts = do
        context <- ask
        return (pullCursor context `fmap` mkAcquire (open context) (close context))
      where
        close :: DB.MongoContext -> DB.Cursor -> IO ()
        close context cursor = runReaderT (DB.closeCursor cursor) context
        open :: DB.MongoContext -> IO DB.Cursor
        open = runReaderT (DB.find (makeQuery filts opts)
                   -- it is an error to apply $snapshot when sorting
                   { DB.snapshot = noSort
                   , DB.options = [DB.NoCursorTimeout]
                   })
        pullCursor context cursor = do
            mdoc <- liftIO $ runReaderT (DB.nextBatch cursor) context
            case mdoc of
                [] -> return ()
                docs -> do
                    forM_ docs $ fromPersistValuesThrow t >=> yield
                    pullCursor context cursor
        t = entityDef $ Just $ dummyFromFilts filts
        (_, _, orders) = limitOffsetOrder opts
        noSort = null orders

    selectFirst filts opts = DB.findOne (makeQuery filts opts)
                         >>= Traversable.mapM (fromPersistValuesThrow t)
      where
        t = entityDef $ Just $ dummyFromFilts filts

    selectKeysRes filts opts = do
        context <- ask
        let make = do
                cursor <- liftIO $ flip runReaderT context $ DB.find $ (makeQuery filts opts) {
                    DB.project = [id_ DB.=: (1 :: Int)]
                  }
                pullCursor context cursor
        return $ return make
      where
        pullCursor context cursor = do
            mdoc <- liftIO $ runReaderT (DB.next cursor) context
            case mdoc of
                Nothing -> return ()
                Just [_id DB.:= idVal] -> do
                    k <- liftIO $ keyFrom_idEx idVal
                    yield k
                    pullCursor context cursor
                Just y -> liftIO $ throwIO $ PersistMarshalError $ T.pack $ "Unexpected in selectKeys: " ++ show y

orderClause :: PersistEntity val => SelectOpt val -> DB.Field
orderClause o = case o of
                  Asc f  -> fieldName f DB.=: ( 1 :: Int)
                  Desc f -> fieldName f DB.=: (-1 :: Int)
                  _      -> error "orderClause: expected Asc or Desc"


makeQuery :: (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext) => [Filter record] -> [SelectOpt record] -> DB.Query
makeQuery filts opts =
    (DB.select (filtersToDoc filts) (collectionName $ dummyFromFilts filts)) {
      DB.limit = fromIntegral limit
    , DB.skip  = fromIntegral offset
    , DB.sort  = orders
    , DB.project = projectionFromRecord (dummyFromFilts filts)
    }
  where
    (limit, offset, orders') = limitOffsetOrder opts
    orders = map orderClause orders'

filtersToDoc :: (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext) => [Filter record] -> DB.Document
filtersToDoc filts =
#ifdef DEBUG
  debug $
#endif
    if null filts then [] else multiFilter AndDollar filts

filterToDocument :: (PersistEntity val, PersistEntityBackend val ~ DB.MongoContext) => Filter val -> DB.Document
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

multiFilter :: forall record. (PersistEntity record, PersistEntityBackend record ~ DB.MongoContext) => MultiFilter -> [Filter record] -> [DB.Field]
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
             -> FilterValue a
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


mongoFilterToBSON :: forall typ. PersistField typ
                  => Text
                  -> MongoFilterOperator typ
                  -> DB.Document
mongoFilterToBSON fname filt = case filt of
    (PersistFilterOperator v op) -> [filterToBSON fname v op]
    (MongoFilterOperator bval)   -> [fname DB.:= bval]

mongoUpdateToBson :: forall typ. PersistField typ
                  => Text
                  -> UpdateValueOp typ
                  -> DB.Field
mongoUpdateToBson fname upd = case upd of
    UpdateValueOp (Left v)  op -> updateToBson fname (toPersistValue v) op
    UpdateValueOp (Right v) op -> updateToBson fname (PersistList $ map toPersistValue v) op

mongoUpdateToDoc :: PersistEntity record => MongoUpdate record -> DB.Field
mongoUpdateToDoc (NestedUpdate   field op) = mongoUpdateToBson (nestedFieldName field) op
mongoUpdateToDoc (ArrayUpdate field op)    = mongoUpdateToBson (fieldName field) op

mongoFilterToDoc :: PersistEntity record => MongoFilter record -> DB.Document
mongoFilterToDoc (NestedFilter   field op) = mongoFilterToBSON (nestedFieldName field) op
mongoFilterToDoc (ArrayFilter field op) = mongoFilterToBSON (fieldName field) op
mongoFilterToDoc (NestedArrayFilter field op) = mongoFilterToBSON (nestedFieldName field) op
mongoFilterToDoc (RegExpFilter fn (reg, opts)) = [ fieldName fn  DB.:= DB.RegEx (DB.Regex reg opts)]

nestedFieldName :: forall record typ. PersistEntity record => NestedField record typ -> Text
nestedFieldName = T.intercalate "." . nesFldName
  where
    nesFldName :: forall r1 r2. (PersistEntity r1) => NestedField r1 r2 -> [DB.Label]
    nesFldName (nf1 `LastEmbFld` nf2)          = [fieldName nf1, fieldName nf2]
    nesFldName ( f1 `MidEmbFld`  f2)           = fieldName f1 : nesFldName f2
    nesFldName ( f1 `MidNestFlds` f2)          = fieldName f1 : nesFldName f2
    nesFldName ( f1 `MidNestFldsNullable` f2)  = fieldName f1 : nesFldName f2
    nesFldName (nf1 `LastNestFld` nf2)         = [fieldName nf1, fieldName nf2]
    nesFldName (nf1 `LastNestFldNullable` nf2) = [fieldName nf1, fieldName nf2]

toValue :: forall a.  PersistField a => FilterValue a -> DB.Value
toValue val =
    case val of
      FilterValue v   -> DB.val $ toPersistValue v
      UnsafeValue v   -> DB.val $ toPersistValue v
      FilterValues vs -> DB.val $ map toPersistValue vs

fieldName ::  forall record typ.  (PersistEntity record) => EntityField record typ -> DB.Label
fieldName f | fieldHaskell fd == HaskellName "Id" = id_
            | otherwise = unDBName $ fieldDB $ fd
  where
    fd = persistFieldDef f

docToEntityEither :: forall record. (PersistEntity record) => DB.Document -> Either T.Text (Entity record)
docToEntityEither doc = entity
  where
    entDef = entityDef $ Just (getType entity)
    entity = eitherFromPersistValues entDef doc
    getType :: Either err (Entity ent) -> ent
    getType = error "docToEntityEither/getType: never here"

docToEntityThrow :: forall m record. (Trans.MonadIO m, PersistEntity record, PersistEntityBackend record ~ DB.MongoContext) => DB.Document -> m (Entity record)
docToEntityThrow doc =
    case docToEntityEither doc of
        Left s -> Trans.liftIO . throwIO $ PersistMarshalError $ s
        Right entity -> return entity


fromPersistValuesThrow :: (Trans.MonadIO m, PersistEntity record, PersistEntityBackend record ~ DB.MongoContext) => EntityDef -> [DB.Field] -> m (Entity record)
fromPersistValuesThrow entDef doc =
    case eitherFromPersistValues entDef doc of
        Left t -> Trans.liftIO . throwIO $ PersistMarshalError $
                   unHaskellName (entityHaskell entDef) `mappend` ": " `mappend` t
        Right entity -> return entity

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _ (Right r) = Right r
mapLeft f (Left l)  = Left (f l)

eitherFromPersistValues :: (PersistEntity record) => EntityDef -> [DB.Field] -> Either T.Text (Entity record)
eitherFromPersistValues entDef doc = case mKey of
   Nothing  -> addDetail $ Left $ "could not find _id field: "
   Just kpv -> do
      body <- addDetail (fromPersistValues (map snd $ orderPersistValues (toEmbedEntityDef entDef) castDoc))
      key <- keyFromValues [kpv]
      return $ Entity key body
  where
    addDetail :: Either Text a -> Either Text a
    addDetail = mapLeft (\msg -> msg `mappend` " for doc: " `mappend` T.pack (show doc))
    castDoc = assocListFromDoc doc
    -- normally _id is the first field
    mKey = lookup id_ castDoc

-- | unlike many SQL databases, MongoDB makes no guarantee of the ordering
-- of the fields returned in the document.
-- Ordering might be maintained if persistent were the only user of the db,
-- but other tools may be using MongoDB.
--
-- Persistent creates a Haskell record from a list of PersistValue
-- But most importantly it puts all PersistValues in the proper order
orderPersistValues :: EmbedEntityDef -> [(Text, PersistValue)] -> [(Text, PersistValue)]
orderPersistValues entDef castDoc = reorder
  where
    castColumns = map nameAndEmbed (embeddedFields entDef)
    nameAndEmbed fdef = (fieldToLabel fdef, emFieldEmbed fdef)

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
        match :: [(Text, Maybe EmbedEntityDef)]
              -> [(Text, PersistValue)]
              -> [(Text, PersistValue)]
              -> [(Text, PersistValue)]
        -- when there are no more Persistent castColumns we are done
        --
        -- allow extra mongoDB fields that persistent does not know about
        -- another application may use fields we don't care about
        -- our own application may set extra fields with the raw driver
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

oidToKey :: (ToBackendKey DB.MongoContext record) => DB.ObjectId -> Key record
oidToKey = fromBackendKey . MongoKey

persistObjectIdToDbOid :: PersistValue -> DB.ObjectId
persistObjectIdToDbOid (PersistObjectId k) = case Serialize.decode k of
                  Left msg -> throw $ PersistError $ T.pack $ "error decoding " ++ (show k) ++ ": " ++ msg
                  Right o -> o
persistObjectIdToDbOid _ = throw $ PersistInvalidField "expected PersistObjectId"

keyToOid :: ToBackendKey DB.MongoContext record => Key record -> DB.ObjectId
keyToOid = unMongoKey . toBackendKey

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
  val (PersistArray a)      = DB.val $ PersistList a
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
    , mgPort     :: DB.PortID
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
defaultAccessMode = DB.ConfirmWrites ["w" DB.:= DB.Int32 1]
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

instance FromJSON MongoConf where
    parseJSON v = modifyFailure ("Persistent: error loading MongoDB conf: " ++) $
      flip (withObject "MongoConf") v $ \o ->do
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

instance PersistConfig MongoConf where
    type PersistConfigBackend MongoConf = DB.Action
    type PersistConfigPool MongoConf = ConnectionPool

    createPoolConfig = createMongoPool

    runPool c = runMongoDBPool (mgAccessMode c)
    loadConfig = parseJSON

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

type instance BackendSpecificFilter DB.MongoContext record = MongoFilter record
type instance BackendSpecificUpdate DB.MongoContext record = MongoUpdate record

data NestedField record typ
  = forall emb.  PersistEntity emb =>  EntityField record [emb] `LastEmbFld` EntityField emb typ
  | forall emb.  PersistEntity emb =>  EntityField record [emb] `MidEmbFld` NestedField emb typ
  | forall nest. PersistEntity nest => EntityField record nest  `MidNestFlds` NestedField nest typ
  | forall nest. PersistEntity nest => EntityField record (Maybe nest) `MidNestFldsNullable` NestedField nest typ
  | forall nest. PersistEntity nest => EntityField record nest `LastNestFld` EntityField nest typ
  | forall nest. PersistEntity nest => EntityField record (Maybe nest) `LastNestFldNullable` EntityField nest typ

-- | A MongoRegex represents a Regular expression.
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
(=~.) :: forall record searchable. (MongoRegexSearchable searchable, PersistEntity record, PersistEntityBackend record ~ DB.MongoContext) => EntityField record searchable -> MongoRegex -> Filter record
fld =~. val = BackendFilter $ RegExpFilter fld val

data MongoFilterOperator typ = PersistFilterOperator (FilterValue typ) PersistFilter
                             | MongoFilterOperator DB.Value

data UpdateValueOp typ =
  UpdateValueOp
    (Either typ [typ])
    (Either PersistUpdate MongoUpdateOperation)
    deriving Show

data MongoUpdateOperation = MongoEach   MongoUpdateOperator
                          | MongoSimple MongoUpdateOperator
                          deriving Show
data MongoUpdateOperator = MongoPush
                         | MongoPull
                         | MongoAddToSet
                         deriving Show

opToText :: MongoUpdateOperator -> Text
opToText MongoPush     = "$push"
opToText MongoPull     = "$pull"
opToText MongoAddToSet = "$addToSet"


data MongoFilter record =
        forall typ. PersistField typ =>
          NestedFilter
            (NestedField record typ)
            (MongoFilterOperator typ)
      | forall typ. PersistField typ =>
          ArrayFilter
            (EntityField record [typ])
            (MongoFilterOperator typ)
      | forall typ. PersistField typ =>
          NestedArrayFilter
            (NestedField record [typ])
            (MongoFilterOperator typ)
      | forall typ. MongoRegexSearchable typ =>
          RegExpFilter
            (EntityField record typ)
            MongoRegex

data MongoUpdate record =
        forall typ. PersistField typ =>
          NestedUpdate
            (NestedField record typ)
            (UpdateValueOp typ)
      | forall typ. PersistField typ =>
          ArrayUpdate
            (EntityField record [typ])
            (UpdateValueOp typ)

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
infixr 4 `nestAnyEq`
infixr 4 `nestBsonEq`
infixr 4 `anyBsonEq`

infixr 4 `nestSet`
infixr 4 `push`
infixr 4 `pull`
infixr 4 `pullAll`
infixr 4 `addToSet`

-- | The normal Persistent equality test '==.' is not generic enough.
-- Instead use this with the drill-down arrow operaters such as '->.'
--
-- using this as the only query filter is similar to the following in the mongoDB shell
--
-- > db.Collection.find({"object.field": item})
nestEq, nestNe, nestGe, nestLe, nestIn, nestNotIn :: forall record typ.
    ( PersistField typ , PersistEntityBackend record ~ DB.MongoContext)
    => NestedField record typ
    -> typ
    -> Filter record
nestEq = nestedFilterOp Eq
nestNe = nestedFilterOp Ne
nestGe = nestedFilterOp Ge
nestLe = nestedFilterOp Le
nestIn = nestedFilterOp In
nestNotIn = nestedFilterOp NotIn

nestedFilterOp :: forall record typ.
       ( PersistField typ
       , PersistEntityBackend record ~ DB.MongoContext
       ) => PersistFilter -> NestedField record typ -> typ -> Filter record
nestedFilterOp op nf v = BackendFilter $
   NestedFilter nf $ PersistFilterOperator (FilterValue v) op

-- | same as `nestEq`, but give a BSON Value
nestBsonEq :: forall record typ.
       ( PersistField typ
       , PersistEntityBackend record ~ DB.MongoContext
       ) => NestedField record typ -> DB.Value -> Filter record
nf `nestBsonEq` val = BackendFilter $
    NestedFilter nf $ MongoFilterOperator val

-- | Like '(==.)' but for an embedded list.
-- Checks to see if the list contains an item.
--
-- In Haskell we need different equality functions for embedded fields that are lists or non-lists to keep things type-safe.
--
-- using this as the only query filter is similar to the following in the mongoDB shell
--
-- > db.Collection.find({arrayField: arrayItem})
anyEq :: forall record typ.
        ( PersistField typ
        , PersistEntityBackend record ~ DB.MongoContext
        ) => EntityField record [typ] -> typ -> Filter record
fld `anyEq` val = BackendFilter $
    ArrayFilter fld $ PersistFilterOperator (FilterValue val) Eq

-- | Like nestEq, but for an embedded list.
-- Checks to see if the nested list contains an item.
nestAnyEq :: forall record typ.
        ( PersistField typ
        , PersistEntityBackend record ~ DB.MongoContext
        ) => NestedField record [typ] -> typ -> Filter record
fld `nestAnyEq` val = BackendFilter $
    NestedArrayFilter fld $ PersistFilterOperator (FilterValue val) Eq

-- | same as `anyEq`, but give a BSON Value
anyBsonEq :: forall record typ.
        ( PersistField typ
        , PersistEntityBackend record ~ DB.MongoContext
        ) => EntityField record [typ] -> DB.Value -> Filter record
fld `anyBsonEq` val = BackendFilter $
    ArrayFilter fld $ MongoFilterOperator val

nestSet, nestInc, nestDec, nestMul :: forall record typ.
    ( PersistField typ , PersistEntityBackend record ~ DB.MongoContext)
    => NestedField record typ
    -> typ
    -> Update record
nestSet = nestedUpdateOp Assign
nestInc = nestedUpdateOp Add
nestDec = nestedUpdateOp Subtract
nestMul = nestedUpdateOp Multiply

push, pull, addToSet :: forall record typ.
        ( PersistField typ
        , PersistEntityBackend record ~ DB.MongoContext
        ) => EntityField record [typ] -> typ -> Update record
fld `push`     val = backendArrayOperation MongoPush     fld val
fld `pull`     val = backendArrayOperation MongoPull     fld val
fld `addToSet` val = backendArrayOperation MongoAddToSet fld val

backendArrayOperation ::
  forall record typ.
  (PersistField typ, BackendSpecificUpdate (PersistEntityBackend record) record ~ MongoUpdate record)
  => MongoUpdateOperator -> EntityField record [typ] -> typ
  -> Update record
backendArrayOperation op fld val = BackendUpdate $
    ArrayUpdate fld $ UpdateValueOp (Left val) (Right $ MongoSimple op)

-- | equivalent to $each
--
-- > eachOp push field []
--
-- @eachOp pull@ will get translated to @$pullAll@
eachOp :: forall record typ.
       ( PersistField typ, PersistEntityBackend record ~ DB.MongoContext)
       => (EntityField record [typ] -> typ -> Update record)
       -> EntityField record [typ] -> [typ]
       -> Update record
eachOp haskellOp fld val = case haskellOp fld (error "eachOp: undefined") of
    BackendUpdate (ArrayUpdate _ (UpdateValueOp (Left _) (Right (MongoSimple op)))) -> each op
    BackendUpdate (ArrayUpdate{})  -> error "eachOp: unexpected ArrayUpdate"
    BackendUpdate (NestedUpdate{}) -> error "eachOp: did not expect NestedUpdate"
    Update{} -> error "eachOp: did not expect Update"
  where
    each op = BackendUpdate $ ArrayUpdate fld $
      UpdateValueOp (Right val) (Right $ MongoEach op)

pullAll :: forall record typ.
        ( PersistField typ
        , PersistEntityBackend record ~ DB.MongoContext
        ) => EntityField record [typ] -> [typ] -> Update record
fld `pullAll` val = eachOp pull fld val


nestedUpdateOp :: forall record typ.
       ( PersistField typ
       , PersistEntityBackend record ~ DB.MongoContext
       ) => PersistUpdate -> NestedField record typ -> typ -> Update record
nestedUpdateOp op nf v = BackendUpdate $
   NestedUpdate nf $ UpdateValueOp (Left v) (Left op)

-- | Intersection of lists: if any value in the field is found in the list.
inList :: PersistField typ => EntityField v [typ] -> [typ] -> Filter v
f `inList` a = Filter (unsafeCoerce f) (FilterValues a) In
infix 4 `inList`

-- | No intersection of lists: if no value in the field is found in the list.
ninList :: PersistField typ => EntityField v [typ] -> [typ] -> Filter v
f `ninList` a = Filter (unsafeCoerce f) (FilterValues a) NotIn
infix 4 `ninList`
