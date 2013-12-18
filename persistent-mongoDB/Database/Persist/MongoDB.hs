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
    , entityToDocument
    , entityToFields
    , toInsertFields
    , docToEntityEither
    , docToEntityThrow

    -- * MongoDB specific Filters
    -- $filters
    , (->.), (~>.), (?&->.), (?&~>.), (&->.), (&~>.)
    , nestEq, multiEq

    -- * MongoDB specific PersistFields
    , Objectid
    , genObjectid

    -- * using connections
    , withMongoDBConn
    , withMongoDBPool
    , createMongoDBPool
    , runMongoDBPool
    , runMongoDBPoolDef
    , ConnectionPool
    , Connection
    , MongoConf (..)
    , MongoBackend
    , MongoAuth (..)
    -- ** using raw MongoDB pipes
    , PipePool
    , createMongoDBPipePool
    , runMongoDBPipePool

    -- * Key conversion helpers
    , keyToOid
    , oidToKey

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

    -- * Database.Persist
    , module Database.Persist
    ) where

import Database.Persist
import qualified Database.Persist.Sql as Sql

import qualified Control.Monad.IO.Class as Trans
import Control.Exception (throw, throwIO)

import qualified Database.MongoDB as DB
import Database.MongoDB.Query (Database)
import Control.Applicative (Applicative)
import Network (PortID (PortNumber))
import Network.Socket (HostName)
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as E
import qualified Data.Serialize as Serialize
import Web.PathPieces (PathPiece (..))
import Data.Conduit
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Object, Number), (.:), (.:?), (.!=), FromJSON(..))
import Control.Monad (mzero, liftM)
import qualified Data.Conduit.Pool as Pool
import Data.Time (NominalDiffTime)
#ifdef HIGH_PRECISION_DATE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
#endif
import Data.Time.Calendar (Day(..))
import Data.Attoparsec.Number
import Data.Char (toUpper)
import Data.Monoid (mappend)
import Data.Typeable
import Data.ByteString (ByteString)

#ifdef DEBUG
import FileLocation (debug)
#endif

newtype NoOrphanNominalDiffTime = NoOrphanNominalDiffTime NominalDiffTime
                                deriving (Show, Eq, Num)

instance FromJSON NoOrphanNominalDiffTime where
    parseJSON (Number (I x)) = (return . NoOrphanNominalDiffTime . fromInteger) x
    parseJSON (Number (D x)) = (return . NoOrphanNominalDiffTime . fromRational . toRational) x
    parseJSON _ = fail "couldn't parse diff time"

newtype NoOrphanPortID = NoOrphanPortID PortID deriving (Show, Eq)

instance FromJSON NoOrphanPortID where
    parseJSON (Number (I x)) = (return . NoOrphanPortID . PortNumber . fromInteger) x
    parseJSON _ = fail "couldn't parse port number"


data Connection = Connection DB.Pipe DB.Database
type ConnectionPool = Pool.Pool Connection

type instance BackendKey MongoBackend = ByteString
{-
-- | ToPathPiece is used to convert a key to/from text
instance PersistEntity record => PathPiece (KeyBackend MongoBackend record) where
    toPathPiece = keyToText
    fromPathPiece keyText = readMayKey $
        -- handle a JSON type prefix
        -- 'o' is a non-hex character, so no confusion here
        case T.uncons keyText of
            Just ('o', prefixed) -> prefixed
            _ -> keyText
-}

keyToText :: PersistEntity record => KeyBackend MongoBackend record -> Text
keyToText = go . persistKeyToPersistValue
  where
    go pOid@(PersistObjectId _) = -- T.pack $ show $ Serialize.encode bsonId
      let oid = persistObjectIdToDbOid pOid
      in  T.pack $ show oid
    go k = throw $ PersistInvalidField $ T.pack $ "Invalid Key (expected PersistObjectId): " ++ show k

-- | Convert a Text to a Key
readMayKey :: PersistEntity record => Text -> Maybe (KeyBackend MongoBackend record)
readMayKey str =
  case (reads $ (T.unpack str)) :: [(DB.ObjectId,String)] of
    (parsed,_):[] -> Just $ persistValueToPersistKey $ PersistObjectId $ Serialize.encode parsed
    _ -> Nothing


-- | wrapper of 'ObjectId'
-- * avoids an orphan instance
-- * works around a Persistent naming issue
newtype Objectid = Objectid { unObjectId :: DB.ObjectId }
                   deriving (Show, Read, Eq, Ord)

-- | like 'genObjectId', but for 'Objectid'
genObjectid :: IO Objectid
genObjectid = Objectid `liftM` DB.genObjectId

instance PersistField Objectid where
    toPersistValue = oidToPersistValue . unObjectId
    fromPersistValue oid@(PersistObjectId _) = Right . Objectid $ persistObjectIdToDbOid oid
    fromPersistValue (PersistByteString bs) = fromPersistValue (PersistObjectId bs)
    fromPersistValue _ = Left $ T.pack "expected PersistObjectId"

instance Sql.PersistFieldSql Objectid where
    sqlType _ = Sql.SqlOther "doesn't make much sense for MongoDB"


withMongoDBConn :: (Trans.MonadIO m, Applicative m)
                => Database -> HostName -> PortID
                -> Maybe MongoAuth -> NominalDiffTime
                -> (ConnectionPool -> m b) -> m b
withMongoDBConn dbname hostname port mauth connectionIdleTime = withMongoDBPool dbname hostname port mauth 1 1 connectionIdleTime

createPipe :: HostName -> PortID -> IO DB.Pipe
createPipe hostname port = DB.runIOE $ DB.connect (DB.Host hostname port)

createConnection :: Database -> HostName -> PortID -> Maybe MongoAuth -> IO Connection
createConnection dbname hostname port mAuth = do
    pipe <- createPipe hostname port
    _ <- case mAuth of
      Just (MongoAuth user pass) -> DB.access pipe DB.UnconfirmedWrites dbname (DB.auth user pass)
      Nothing -> return undefined
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
createMongoDBPipePool hostname port connectionPoolSize stripeSize connectionIdleTime = do
  Trans.liftIO $ Pool.createPool
                          (createPipe hostname port)
                          (\pipe -> DB.close pipe)
                          connectionPoolSize
                          connectionIdleTime
                          stripeSize

withMongoDBPool :: (Trans.MonadIO m, Applicative m) =>
  Database -> HostName -> PortID -> Maybe MongoAuth -> Int -> Int -> NominalDiffTime -> (ConnectionPool -> m b) -> m b
withMongoDBPool dbname hostname port mauth poolStripes stripeConnections connectionIdleTime connectionReader = do
  pool <- createMongoDBPool dbname hostname port mauth poolStripes stripeConnections connectionIdleTime
  connectionReader pool

-- | run a pool created with 'createMongoDBPipePool'
runMongoDBPipePool :: (Trans.MonadIO m, MonadBaseControl IO m) => DB.AccessMode -> Database -> DB.Action m a -> PipePool -> m a
runMongoDBPipePool accessMode db action pool =
  Pool.withResource pool $ \pipe -> do
    res  <- DB.access pipe accessMode db action
    either (Trans.liftIO . throwIO . PersistMongoDBError . T.pack . show) return res

runMongoDBPool :: (Trans.MonadIO m, MonadBaseControl IO m) => DB.AccessMode  -> DB.Action m a -> ConnectionPool -> m a
runMongoDBPool accessMode action pool =
  Pool.withResource pool $ \(Connection pipe db) -> do
    res  <- DB.access pipe accessMode db action
    either (Trans.liftIO . throwIO . PersistMongoDBError . T.pack . show) return res


-- | use default 'AccessMode'
runMongoDBPoolDef :: (Trans.MonadIO m, MonadBaseControl IO m) => DB.Action m a -> ConnectionPool -> m a
runMongoDBPoolDef = runMongoDBPool (DB.ConfirmWrites ["j" DB.=: True])

filterByKey :: (PersistEntity record, EntityBackend record ~ MongoBackend)
            => Key record -> DB.Document
filterByKey k = [_id DB.=: keyToOid k]

queryByKey :: (PersistEntity record, EntityBackend record ~ MongoBackend)
           => Key record -> EntityDef a -> DB.Query
queryByKey k record = (DB.select (filterByKey k) (unDBName $ entityDB record))

selectByKey :: (PersistEntity record, EntityBackend record ~ MongoBackend)
            => Key record -> EntityDef a -> DB.Selection
selectByKey k record = (DB.select (filterByKey k) (unDBName $ entityDB record))

updateFields :: (PersistEntity entity) => [Update entity] -> [DB.Field]
updateFields upds = map updateToMongoField upds 

updateToMongoField :: (PersistEntity entity) => Update entity -> DB.Field
updateToMongoField (Update field v up) =
    opName DB.:= DB.Doc [( (unDBName $ fieldDB $ persistFieldDef field) DB.:= opValue)]
    where 
      (opName, opValue) =
        case (up, toPersistValue v) of
                  (Assign, PersistNull) -> ("$unset", DB.Int64 1)
                  (Assign,a)    -> ("$set", DB.val a)
                  (Add, a)      -> ("$inc", DB.val a)
                  (Subtract, PersistInt64 i) -> ("$inc", DB.Int64 (-i))
                  (Subtract, _) -> error "expected PersistInt64 for a subtraction"
                  (Multiply, _) -> throw $ PersistMongoDBUnsupported "multiply not supported"
                  (Divide, _)   -> throw $ PersistMongoDBUnsupported "divide not supported"


uniqSelector :: forall record.  (PersistEntity record) => Unique record -> [DB.Field]
uniqSelector uniq = zipWith (DB.:=)
  (map (unDBName . snd) $ persistUniqueToFieldNames uniq)
  (map DB.val (persistUniqueToValues uniq))

-- | convert a PersistEntity into document fields.
-- for inserts only: nulls are ignored so they will be unset in the document.
-- 'entityToFields' includes nulls
toInsertFields :: forall record.  (PersistEntity record) => record -> [DB.Field]
toInsertFields record = zipFilter (entityFields entity) (toPersistFields record)
  where
    zipFilter [] _  = []
    zipFilter _  [] = []
    zipFilter (e:efields) (p:pfields) = let pv = toPersistValue p in
        if pv == PersistNull then zipFilter efields pfields
          else (fieldToLabel e DB.:= DB.val pv):zipFilter efields pfields
    entity = entityDef $ Just record

collectionName :: (PersistEntity record) => record -> Text
collectionName = unDBName . entityDB . entityDef . Just

-- | convert a PersistEntity into document fields.
-- unlike 'toInsertFields', nulls are included.
entityToDocument :: (PersistEntity record) => record -> [DB.Field]
entityToDocument record = zipIt (entityFields entity) (toPersistFields record)
  where
    zipIt [] _  = []
    zipIt _  [] = []
    zipIt (e:efields) (p:pfields) =
      let pv = toPersistValue p
      in  (fieldToLabel e DB.:= DB.val pv):zipIt efields pfields
    entity = entityDef $ Just record

-- | Deprecated, use the better named entityToDocument
entityToFields :: (PersistEntity record) => record -> [DB.Field]
entityToFields = entityToDocument
{-# DEPRECATED entityToFields "Please use entityToDocument instead" #-}

fieldToLabel :: FieldDef a -> Text
fieldToLabel = unDBName . fieldDB

saveWithKey :: forall m entity keyEntity.
            (PersistEntity entity, PersistEntity keyEntity, EntityBackend keyEntity ~ MongoBackend)
            => (entity -> [DB.Field])
            -> (Text -> [DB.Field] -> DB.Action m ())
            -> Key keyEntity
            -> entity
            -> DB.Action m ()
saveWithKey entToFields dbSave key record =
      dbSave (collectionName record) ((keyToMongoIdField key):(entToFields record))

data MongoBackend deriving Typeable

instance (Applicative m, Functor m, Trans.MonadIO m, MonadBaseControl IO m) => PersistStore (DB.Action m) where
    type MonadBackend (DB.Action m) = MongoBackend

    insert record = do
        DB.ObjId oid <- DB.insert (collectionName record) (toInsertFields record)
        return $ oidToKey oid 

    insertMany [] = return []
    insertMany (r:records) = map (\(DB.ObjId oid) -> oidToKey oid) `fmap`
        DB.insertMany (collectionName r) (map toInsertFields (r:records))

    insertKey k record = saveWithKey toInsertFields DB.insert_ k record

    repsert   k record = saveWithKey entityToDocument DB.save k record

    replace k record = do
        DB.replace (selectByKey k t) (toInsertFields record)
        return ()
      where
        t = entityDef $ Just record

    delete k =
        DB.deleteOne DB.Select {
          DB.coll = collectionName (dummyFromKey k)
        , DB.selector = filterByKey k
        }

    get k = do
            d <- DB.findOne (queryByKey k t)
            case d of
              Nothing -> return Nothing
              Just doc -> do
                Entity _ ent <- fromPersistValuesThrow t doc
                return $ Just ent
          where
            t = entityDef $ Just $ dummyFromKey k

instance MonadThrow m => MonadThrow (DB.Action m) where
    monadThrow = lift . monadThrow

instance (Applicative m, Functor m, Trans.MonadIO m, MonadBaseControl IO m) => PersistUnique (DB.Action m) where
    getBy uniq = do
        mdoc <- DB.findOne $
          DB.select (uniqSelector uniq) (unDBName $ entityDB t)
        case mdoc of
            Nothing -> return Nothing
            Just doc -> fmap Just $ fromPersistValuesThrow t doc
      where
        t = entityDef $ Just $ dummyFromUnique uniq

    deleteBy uniq =
        DB.delete DB.Select {
          DB.coll = collectionName $ dummyFromUnique uniq
        , DB.selector = uniqSelector uniq
        }

_id :: T.Text
_id = "_id"

keyToMongoIdField :: (PersistEntity entity, EntityBackend entity ~ MongoBackend)
                  => Key entity -> DB.Field
keyToMongoIdField k = _id DB.:= (DB.ObjId $ keyToOid k)


instance (Applicative m, Functor m, Trans.MonadIO m, MonadBaseControl IO m) => PersistQuery (DB.Action m) where
    update _ [] = return ()
    update key upds =
        DB.modify 
           (DB.Select [keyToMongoIdField key] (collectionName $ dummyFromKey key))
           $ updateFields upds

    updateGet key upds = do
        result <- DB.findAndModify (DB.select [keyToMongoIdField key]
                     (unDBName $ entityDB t)
                   ) (updateFields upds)
        case result of
          Left e -> err e
          Right doc -> do
            Entity _ ent <- fromPersistValuesThrow t doc
            return ent
      where
        err msg = Trans.liftIO $ throwIO $ KeyNotFound $ show key ++ msg
        t = entityDef $ Just $ dummyFromKey key


    updateWhere _ [] = return ()
    updateWhere filts upds =
        DB.modify DB.Select {
          DB.coll = collectionName $ dummyFromFilts filts
        , DB.selector = filtersToSelector filts
        } $ updateFields upds

    deleteWhere filts = do
        DB.delete DB.Select {
          DB.coll = collectionName $ dummyFromFilts filts
        , DB.selector = filtersToSelector filts
        }

    count filts = do
        i <- DB.count query
        return $ fromIntegral i
      where
        query = DB.select (filtersToSelector filts) $
                  collectionName $ dummyFromFilts filts

    selectSource filts opts = do
        cursor <- lift $ DB.find $ makeQuery filts opts
        pull cursor
      where
        pull cursor = do
            mdoc <- lift $ DB.next cursor
            case mdoc of
                Nothing -> return ()
                Just doc -> do
                    entity <- fromPersistValuesThrow t doc
                    yield entity
                    pull cursor
        t = entityDef $ Just $ dummyFromFilts filts

    selectFirst filts opts = do
        mdoc <- DB.findOne $ makeQuery filts opts
        case mdoc of
            Nothing -> return Nothing
            Just doc -> fmap Just $ fromPersistValuesThrow t doc
      where
        t = entityDef $ Just $ dummyFromFilts filts

    selectKeys filts opts = do
        cursor <- lift $ DB.find $ (makeQuery filts opts) {
            DB.project = [_id DB.=: (1 :: Int)]
          }
        pull cursor
      where
        pull cursor = do
            mdoc <- lift $ DB.next cursor
            case mdoc of
                Nothing -> return ()
                Just [_id DB.:= DB.ObjId oid] -> do
                    yield $ oidToKey oid
                    pull cursor
                Just y -> liftIO $ throwIO $ PersistMarshalError $ T.pack $ "Unexpected in selectKeys: " ++ show y

orderClause :: PersistEntity val => SelectOpt val -> DB.Field
orderClause o = case o of
                  Asc f  -> fieldName f DB.=: ( 1 :: Int)
                  Desc f -> fieldName f DB.=: (-1 :: Int)
                  _      -> error "orderClause: expected Asc or Desc"


makeQuery :: (PersistEntity val, EntityBackend val ~ MongoBackend) => [Filter val] -> [SelectOpt val] -> DB.Query
makeQuery filts opts =
    (DB.select (filtersToSelector filts) (collectionName $ dummyFromFilts filts)) {
      DB.limit = fromIntegral limit
    , DB.skip  = fromIntegral offset
    , DB.sort  = orders
    }
  where
    (limit, offset, orders') = limitOffsetOrder opts
    orders = map orderClause orders'

filtersToSelector :: (PersistEntity val, EntityBackend val ~ MongoBackend) => [Filter val] -> DB.Document
filtersToSelector filts = 
#ifdef DEBUG
  debug $
#endif
    if null filts then [] else concatMap filterToDocument filts

multiFilter :: forall record. (PersistEntity record, EntityBackend record ~ MongoBackend) => String -> [Filter record] -> [DB.Field]
multiFilter multi fs = [T.pack multi DB.:= DB.Array (map (DB.Doc . filterToDocument) fs)]

filterToDocument :: (PersistEntity val, EntityBackend val ~ MongoBackend) => Filter val -> DB.Document
filterToDocument f =
    case f of
      Filter field v filt -> return $ case filt of
          Eq -> fieldName field DB.:= toValue v
          _  -> fieldName field DB.=: [(showFilter filt) DB.:= toValue v]
      FilterOr [] -> -- Michael decided to follow Haskell's semantics, which seems reasonable to me.
                     -- in Haskell an empty or is a False
                     -- Perhaps there is a less hacky way of creating a query that always returns false?
                     ["$not" DB.=: ["$exists" DB.=: _id]]
      FilterOr fs  -> multiFilter "$or" fs
      -- usually $and is unecessary, but it makes query construction easier in special cases
      FilterAnd [] -> []
      FilterAnd fs -> multiFilter "$and" fs
      BackendFilter mf -> mongoFilterToDoc mf
  where
    showFilter Ne = "$ne"
    showFilter Gt = "$gt"
    showFilter Lt = "$lt"
    showFilter Ge = "$gte"
    showFilter Le = "$lte"
    showFilter In = "$in"
    showFilter NotIn = "$nin"
    showFilter Eq = error "EQ filter not expected"
    showFilter (BackendSpecificFilter bsf) = throw $ PersistMongoDBError $ T.pack $ "did not expect BackendSpecificFilter " ++ T.unpack bsf

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

docToEntityThrow :: forall m record. (Trans.MonadIO m, PersistEntity record) => DB.Document -> m (Entity record)
docToEntityThrow doc =
    case docToEntityEither doc of
        Left s -> Trans.liftIO . throwIO $ PersistMarshalError $ s
        Right entity -> return entity


fromPersistValuesThrow :: (Trans.MonadIO m, PersistEntity record) => EntityDef a -> [DB.Field] -> m (Entity record)
fromPersistValuesThrow entDef doc = 
    case eitherFromPersistValues entDef doc of
        Left t -> Trans.liftIO . throwIO $ PersistMarshalError $
                   unHaskellName (entityHaskell entDef) `mappend` ": " `mappend` t
        Right entity -> return entity

eitherFromPersistValues :: (PersistEntity record) => EntityDef a -> [DB.Field] -> Either T.Text (Entity record)
eitherFromPersistValues entDef doc =
    let castDoc = assocListFromDoc doc
        -- normally _id is the first field
        mKey = lookup _id castDoc
    in case mKey of
         Nothing -> Left "could not find _id field"
         Just key -> case fromPersistValues (map snd $ orderPersistValues entDef castDoc) of
             Right body -> Right $ Entity (persistValueToPersistKey key) body
             Left e -> Left e

-- | unlike many SQL databases, MongoDB makes no guarantee of the ordering
-- of the fields returned in the document.
-- Ordering might be maintained if persistent were the only user of the db,
-- but other tools may be using MongoDB.
--
-- Persistent creates a Haskell record from a list of PersistValue
-- But most importantly it puts all PersistValues in the proper order
orderPersistValues :: EntityDef a -> [(Text, PersistValue)] -> [(Text, PersistValue)]
orderPersistValues entDef castDoc = reorder
  where
    castColumns = map nameAndEmbedded (entityFields entDef)
    nameAndEmbedded fdef = ((unDBName . fieldDB) fdef, fieldEmbedded fdef)

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
        match :: [(Text, Maybe (EntityDef ()) )]
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
            nestedOrder (Just ent) (PersistMap m) =
              PersistMap $ orderPersistValues ent m
            nestedOrder (Just ent) (PersistList l) =
              PersistList $ map (nestedOrder (Just ent)) l
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
assocListFromDoc = Prelude.map (\f -> ( (DB.label f), (fromJust . DB.cast') (DB.value f) ) )

oidToPersistValue :: DB.ObjectId -> PersistValue
oidToPersistValue = PersistObjectId . Serialize.encode

oidToKey :: (PersistEntity entity) => DB.ObjectId -> Key entity
oidToKey = persistValueToPersistKey . oidToPersistValue

persistObjectIdToDbOid :: PersistValue -> DB.ObjectId
persistObjectIdToDbOid (PersistObjectId k) = case Serialize.decode k of
                  Left msg -> throw $ PersistError $ T.pack $ "error decoding " ++ (show k) ++ ": " ++ msg
                  Right o -> o
persistObjectIdToDbOid _ = throw $ PersistInvalidField "expected PersistObjectId"

keyToOid :: (PersistEntity entity, EntityBackend entity ~ MongoBackend)
         => Key entity -> DB.ObjectId
keyToOid = persistObjectIdToDbOid . persistKeyToPersistValue

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
  val (PersistZonedTime (ZT x)) = DB.String $ T.pack $ show x
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

instance Serialize.Serialize DB.ObjectId where
  put (DB.Oid w1 w2) = do Serialize.put w1
                          Serialize.put w2

  get = do w1 <- Serialize.get
           w2 <- Serialize.get
           return (DB.Oid w1 w2) 

dummyFromKey :: KeyBackend MongoBackend v -> v
dummyFromKey _ = error "dummyFromKey"
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
    } deriving Show

instance PersistConfig MongoConf where
    type PersistConfigBackend MongoConf = DB.Action
    type PersistConfigPool MongoConf = ConnectionPool

    createPoolConfig c =
      createMongoDBPool 
         (mgDatabase c) (T.unpack (mgHost c)) (mgPort c)
         (mgAuth c)
         (mgPoolStripes c) (mgStripeConnections c) (mgConnectionIdleTime c)

    runPool c = runMongoDBPool (mgAccessMode c)
    loadConfig (Object o) = do
        db                 <- o .:  "database"
        host               <- o .:? "host" .!= "127.0.0.1"
        (NoOrphanPortID port) <- o .:? "port" .!= (NoOrphanPortID DB.defaultPort)
        poolStripes        <- o .:? "poolstripes" .!= 1
        stripeConnections  <- o .:  "connections"
        (NoOrphanNominalDiffTime connectionIdleTime) <- o .:? "connectionIdleTime" .!= 20
        mUser              <- o .:? "user"
        mPass              <- o .:? "password"
        accessString       <- o .:? "accessMode" .!= "ConfirmWrites"

        mPoolSize         <- o .:? "poolsize"
        case mPoolSize of
          Nothing -> return ()
          Just (_::Int) -> fail "specified deprecated poolsize attribute. Please specify a connections. You can also specify a pools attribute which defaults to 1. Total connections opened to the db are connections * pools"

        accessMode <- case accessString of
               "ReadStaleOk"       -> return DB.ReadStaleOk
               "UnconfirmedWrites" -> return DB.UnconfirmedWrites
               "ConfirmWrites"     -> return $ DB.ConfirmWrites ["j" DB.=: True]
               badAccess -> fail $ "unknown accessMode: " ++ (T.unpack badAccess)

        return $ MongoConf {
            mgDatabase = db
          , mgHost = host
          , mgPort = port
          , mgAuth =
              (case (mUser, mPass) of
                (Just user, Just pass) -> Just (MongoAuth user pass)
                _ -> Nothing
              )
          , mgPoolStripes = poolStripes
          , mgStripeConnections = stripeConnections
          , mgAccessMode = accessMode
          , mgConnectionIdleTime = connectionIdleTime
          }
      where
    {-
        safeRead :: String -> T.Text -> MEither String Int
        safeRead name t = case reads s of
            (i, _):_ -> MRight i
            []       -> MLeft $ concat ["Invalid value for ", name, ": ", s]
          where
            s = T.unpack t
            -}
    loadConfig _ = mzero


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
data MongoFilter record = forall typ. (PersistField typ) =>
                        NestedFilter {
                          nestedField :: NestedField record typ
                        , fieldValue  :: Either typ [typ]
                        }
                      | forall typ. PersistField typ =>
                        MultiKeyFilter {
                          mulFldKey  :: EntityField record [typ]
                        , mulFldVal  :: Either typ [typ]
                        }

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


infixr 5 ~>.
infixr 5 &~>.
infixr 5 ?&~>.
infixr 6 &->.
infixr 6 ?&->.
infixr 6 ->.

infixr 4 `nestEq`

-- | The normal Persistent equality test (==.) is not generic enough.
-- Instead use this with the drill-down operaters (->.) or (?->.)
nestEq :: forall v typ. (PersistField typ, EntityBackend v ~ MongoBackend) => NestedField v typ -> typ -> Filter v
nf `nestEq` v = BackendFilter $ NestedFilter {nestedField = nf, fieldValue = (Left v)}

-- | use to see if an embedded list contains an item
multiEq :: forall v typ. (PersistField typ, EntityBackend v ~ MongoBackend) => EntityField v [typ] -> typ -> Filter v
fld `multiEq` val = BackendFilter $ MultiKeyFilter {mulFldKey = fld, mulFldVal = (Left val)}

mongoFilterToDoc :: PersistEntity val => MongoFilter val -> DB.Document
mongoFilterToDoc (MultiKeyFilter fn v) = return (fieldName fn DB.:= toValue v)
mongoFilterToDoc (NestedFilter fns v) = return ( (nesFldName fns) DB.:= toValue v)
    where
      nesFldName fns' = T.intercalate "." $ nesIdFix . reverse $ nesFldName' fns' []
      nesFldName' :: forall r1 r2. (PersistEntity r1) => NestedField r1 r2 -> [DB.Label] -> [DB.Label]
      nesFldName' (nf1 `LastEmbFld` nf2)          lbls = fieldName nf2 : fieldName nf1 : lbls
      nesFldName' ( f1 `MidEmbFld`  f2)           lbls = nesFldName' f2 (fieldName f1 : lbls)
      nesFldName' ( f1 `MidNestFlds` f2)          lbls = nesFldName' f2 (fieldName f1 : lbls)
      nesFldName' ( f1 `MidNestFldsNullable` f2)  lbls = nesFldName' f2 (fieldName f1 : lbls)
      nesFldName' (nf1 `LastNestFld` nf2)         lbls = fieldName nf2 : fieldName nf1:lbls
      nesFldName' (nf1 `LastNestFldNullable` nf2) lbls = fieldName nf2 : fieldName nf1:lbls
      nesIdFix [] = []
      nesIdFix (fst':rst') = fst': (map (joinFN . (T.splitOn "_")) rst')
      joinFN :: [Text] -> Text
      joinFN [] = ""
      joinFN (fst':rst') = fst' `T.append` (T.concat (map (\t -> (toUpper . T.head $ t) `T.cons` (T.tail t)) rst'))
