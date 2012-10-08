{-# LANGUAGE CPP, PackageImports, OverloadedStrings, ScopedTypeVariables  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE RankNTypes, TypeFamilies #-}

{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Database.Persist.MongoDB
    (
    -- * using connections
      withMongoDBConn
    , withMongoDBPool
    , createMongoDBPool
    , runMongoDBPool
    , runMongoDBPoolDef
    , ConnectionPool
    , Connection
    , MongoConf (..)
    -- * Key conversion helpers
    , keyToOid
    , oidToKey
    -- * Entity conversion
    , entityToFields
    , toInsertFields
    , docToEntityEither
    , docToEntityThrow
    -- * network type
    , HostName
    , PortID
    -- * MongoDB driver types
    , DB.Action
    , DB.AccessMode(..)
    , DB.master
    , DB.slaveOk
    , (DB.=:)
    -- * Database.Persist
    , module Database.Persist
    -- * Mongo Filters
    , (~>.) ,(?~>.), (->.), (?->.)
    , nestEq, multiEq
    ) where

import Database.Persist
import Database.Persist.EntityDef
import Database.Persist.Store
import Database.Persist.Query.Internal

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
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Conduit.Pool as Pool
import Data.Time (NominalDiffTime)
import Data.Attoparsec.Number
import Data.Char (toUpper)

#ifdef DEBUG
import FileLocation (debug)
#endif

newtype NoOrphanNominalDiffTime = NoOrphanNominalDiffTime NominalDiffTime
                                deriving (Show, Eq, Num)

instance FromJSON NoOrphanNominalDiffTime where
    parseJSON (Number (I x)) = (return . NoOrphanNominalDiffTime . fromInteger) x
    parseJSON (Number (D x)) = (return . NoOrphanNominalDiffTime . fromRational . toRational) x
    parseJSON _ = fail "couldn't parse diff time"

instance FromJSON PortID where
    parseJSON (Number (I x)) = (return . PortNumber . fromInteger) x
    parseJSON _ = fail "couldn't parse port number"

data Connection = Connection DB.Pipe DB.Database
type ConnectionPool = Pool.Pool Connection

instance PathPiece (Key DB.Action entity) where
    toPathPiece (Key pOid@(PersistObjectId _)) = -- T.pack $ show $ Serialize.encode bsonId
        let oid = persistObjectIdToDbOid pOid
        in  T.pack $ show oid
    toPathPiece k = throw $ PersistInvalidField $ T.pack $ "Invalid Key (expected PersistObjectId): " ++ show k

    fromPathPiece str =
      case (reads $ (T.unpack str))::[(DB.ObjectId,String)] of
        (parsed,_):[] -> Just $ Key $ PersistObjectId $ Serialize.encode parsed
        _ -> Nothing


withMongoDBConn :: (Trans.MonadIO m, Applicative m) =>
  Database -> HostName -> PortID -> Maybe MongoAuth -> NominalDiffTime -> (ConnectionPool -> m b) -> m b
withMongoDBConn dbname hostname port mauth connectionIdleTime = withMongoDBPool dbname hostname port mauth 1 1 connectionIdleTime

createConnection :: Database -> HostName -> PortID -> Maybe MongoAuth -> IO Connection
createConnection dbname hostname port mAuth = do
    pipe <- DB.runIOE $ DB.connect (DB.Host hostname port)
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

withMongoDBPool :: (Trans.MonadIO m, Applicative m) =>
  Database -> HostName -> PortID -> Maybe MongoAuth -> Int -> Int -> NominalDiffTime -> (ConnectionPool -> m b) -> m b
withMongoDBPool dbname hostname port mauth poolStripes stripeConnections connectionIdleTime connectionReader = do
  pool <- createMongoDBPool dbname hostname port mauth poolStripes stripeConnections connectionIdleTime
  connectionReader pool

runMongoDBPool :: (Trans.MonadIO m, MonadBaseControl IO m) => DB.AccessMode  -> DB.Action m a -> ConnectionPool -> m a
runMongoDBPool accessMode action pool =
  Pool.withResource pool $ \(Connection pipe db) -> do
    res  <- DB.access pipe accessMode db action
    either (Trans.liftIO . throwIO . PersistMongoDBError . T.pack . show) return res

runMongoDBPoolDef :: (Trans.MonadIO m, MonadBaseControl IO m) => DB.Action m a -> ConnectionPool -> m a
runMongoDBPoolDef = runMongoDBPool (DB.ConfirmWrites ["j" DB.=: True])

filterByKey :: (PersistEntity val) => Key DB.Action val -> DB.Document
filterByKey k = [_id DB.=: keyToOid k]

queryByKey :: (PersistEntity val) => Key DB.Action val -> EntityDef -> DB.Query 
queryByKey k entity = (DB.select (filterByKey k) (unDBName $ entityDB entity)) 

selectByKey :: (PersistEntity val) => Key DB.Action val -> EntityDef -> DB.Selection 
selectByKey k entity = (DB.select (filterByKey k) (unDBName $ entityDB entity))

updateFields :: (PersistEntity val) => [Update val] -> [DB.Field]
updateFields upds = map updateToMongoField upds 

updateToMongoField :: (PersistEntity val) => Update val -> DB.Field
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


uniqSelector :: forall record.  (PersistEntity record) => Unique record DB.Action -> [DB.Field]
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
          else (toLabel e DB.:= DB.val pv):zipFilter efields pfields
    entity = entityDef record

-- | convert a PersistEntity into document fields.
-- unlike 'toInsertFields', nulls are included.
entityToFields :: forall record.  (PersistEntity record) => record -> [DB.Field]
entityToFields record = zipIt (entityFields entity) (toPersistFields record)
  where
    zipIt [] _  = []
    zipIt _  [] = []
    zipIt (e:efields) (p:pfields) =
      let pv = toPersistValue p
      in  (toLabel e DB.:= DB.val pv):zipIt efields pfields
    entity = entityDef record

toLabel :: FieldDef -> Text
toLabel = unDBName . fieldDB

saveWithKey :: forall m record keyEntity. -- (Applicative m, Functor m, MonadBaseControl IO m,
                                    (PersistEntity keyEntity, PersistEntity record)
            => (record -> [DB.Field])
            -> (DB.Collection -> DB.Document -> DB.Action m () )
            -> Key DB.Action keyEntity
            -> record
            -> DB.Action m ()
saveWithKey entToFields dbSave key record =
      dbSave (unDBName $ entityDB entity) ((keyToMongoIdField key):(entToFields record))
    where
      entity = entityDef record

instance (Applicative m, Functor m, Trans.MonadIO m, MonadBaseControl IO m) => PersistStore DB.Action m where
    insert record = do
        DB.ObjId oid <- DB.insert (unDBName $ entityDB entity) (toInsertFields record)
        return $ oidToKey oid 
      where
        entity = entityDef record

    insertKey k record = saveWithKey toInsertFields DB.insert_ k record

    repsert   k record = saveWithKey entityToFields DB.save k record

    replace k record = do
        DB.replace (selectByKey k t) (toInsertFields record)
        return ()
      where
        t = entityDef record

    delete k =
        DB.deleteOne DB.Select {
          DB.coll = (unDBName $ entityDB t)
        , DB.selector = filterByKey k
        }
      where
        t = entityDef $ dummyFromKey k

    get k = do
            d <- DB.findOne (queryByKey k t)
            case d of
              Nothing -> return Nothing
              Just doc -> do
                Entity _ ent <- fromPersistValuesThrow t doc
                return $ Just ent
          where
            t = entityDef $ dummyFromKey k

instance MonadThrow m => MonadThrow (DB.Action m) where
    monadThrow = lift . monadThrow

instance (Applicative m, Functor m, Trans.MonadIO m, MonadBaseControl IO m) => PersistUnique DB.Action m where
    getBy uniq = do
        mdoc <- DB.findOne $
          (DB.select (uniqSelector uniq) (unDBName $ entityDB t))
        case mdoc of
            Nothing -> return Nothing
            Just doc -> fmap Just $ fromPersistValuesThrow t doc
      where
        t = entityDef $ dummyFromUnique uniq

    deleteBy uniq =
        DB.delete DB.Select {
          DB.coll = unDBName $ entityDB t
        , DB.selector = uniqSelector uniq
        }
      where
        t = entityDef $ dummyFromUnique uniq

_id :: T.Text
_id = "_id"

keyToMongoIdField :: PersistEntity val => Key DB.Action val -> DB.Field
keyToMongoIdField k = _id DB.:= (DB.ObjId $ keyToOid k)


findAndModifyOne :: (Applicative m, Trans.MonadIO m)
                 => DB.Collection
                 -> DB.ObjectId -- ^ _id for query
                 -> [DB.Field]  -- ^ updates
                 -> DB.Action m (Either String DB.Document)
findAndModifyOne coll objectId updates = do
  result <- DB.runCommand [
     "findAndModify" DB.:= DB.String coll,
     "new" DB.:= DB.Bool True, -- return updated document, not original document
     "query" DB.:= DB.Doc [_id DB.:= DB.ObjId objectId],
     "update" DB.:= DB.Doc updates
   ]
  return $ case DB.lookup "err" (DB.at "lastErrorObject" result) result of
    Just e -> Left e
    Nothing -> case DB.lookup "value" result of
      Nothing -> Left "no value field"
      Just doc -> Right doc

instance (Applicative m, Functor m, Trans.MonadIO m, MonadBaseControl IO m) => PersistQuery DB.Action m where
    update _ [] = return ()
    update key upds =
        DB.modify 
           (DB.Select [keyToMongoIdField key] (unDBName $ entityDB t))
           $ updateFields upds
      where
        t = entityDef $ dummyFromKey key

    updateGet key upds = do
        result <- findAndModifyOne (unDBName $ entityDB t)
                   (keyToOid key) (updateFields upds)
        case result of
          Left e -> err e
          Right doc -> do
            Entity _ ent <- fromPersistValuesThrow t doc
            return ent
      where
        err msg = Trans.liftIO $ throwIO $ KeyNotFound $ show key ++ msg
        t = entityDef $ dummyFromKey key


    updateWhere _ [] = return ()
    updateWhere filts upds =
        DB.modify DB.Select {
          DB.coll = (unDBName $ entityDB t)
        , DB.selector = filtersToSelector filts
        } $ updateFields upds
      where
        t = entityDef $ dummyFromFilts filts

    deleteWhere filts = do
        DB.delete DB.Select {
          DB.coll = (unDBName $ entityDB t)
        , DB.selector = filtersToSelector filts
        }
      where
        t = entityDef $ dummyFromFilts filts

    count filts = do
        i <- DB.count query
        return $ fromIntegral i
      where
        query = DB.select (filtersToSelector filts) (unDBName $ entityDB t)
        t = entityDef $ dummyFromFilts filts

    selectSource filts opts = do
        cursor <- lift $ lift $ DB.find $ makeQuery filts opts
        pull cursor
      where
        pull cursor = do
            mdoc <- lift $ lift $ DB.next cursor
            case mdoc of
                Nothing -> return ()
                Just doc -> do
                    entity <- fromPersistValuesThrow t doc
                    yield entity
                    pull cursor
        t = entityDef $ dummyFromFilts filts

    selectFirst filts opts = do
        mdoc <- DB.findOne $ makeQuery filts opts
        case mdoc of
            Nothing -> return Nothing
            Just doc -> fmap Just $ fromPersistValuesThrow t doc
      where
        t = entityDef $ dummyFromFilts filts

    selectKeys filts opts = do
        cursor <- lift $ lift $ DB.find $ (makeQuery filts opts) {
            DB.project = [_id DB.=: (1 :: Int)]
          }
        pull cursor
      where
        pull cursor = do
            mdoc <- lift $ lift $ DB.next cursor
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


makeQuery :: (PersistEntity val, PersistEntityBackend val ~ DB.Action) => [Filter val] -> [SelectOpt val] -> DB.Query
makeQuery filts opts =
    (DB.select (filtersToSelector filts) (unDBName $ entityDB t)) {
      DB.limit = fromIntegral limit
    , DB.skip  = fromIntegral offset
    , DB.sort  = orders
    }
  where
    t = entityDef $ dummyFromFilts filts
    (limit, offset, orders') = limitOffsetOrder opts
    orders = map orderClause orders'

filtersToSelector :: (PersistEntity val, PersistEntityBackend val ~ DB.Action) => [Filter val] -> DB.Document
filtersToSelector filts = 
#ifdef DEBUG
  debug $
#endif
    if null filts then [] else concatMap filterToDocument filts

multiFilter :: forall record. (PersistEntity record, PersistEntityBackend record ~ DB.Action) => String -> [Filter record] -> [DB.Field]
multiFilter multi fs = [T.pack multi DB.:= DB.Array (map (DB.Doc . filterToDocument) fs)]

filterToDocument :: (PersistEntity val, PersistEntityBackend val ~ DB.Action) => Filter val -> DB.Document
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
      -- $and is usually unecessary but makes query construction easier in special cases
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
    entDef = entityDef (getType entity)
    entity = eitherFromPersistValues entDef doc
    getType :: Either err (Entity ent) -> ent
    getType = error "docToEntityEither/getType: never here"

docToEntityThrow :: forall m record. (Trans.MonadIO m, PersistEntity record) => DB.Document -> m (Entity record)
docToEntityThrow doc =
    case docToEntityEither doc of
        Left s -> Trans.liftIO . throwIO $ PersistMarshalError $ s
        Right entity -> return entity


fromPersistValuesThrow :: (Trans.MonadIO m, PersistEntity record) => EntityDef -> [DB.Field] -> m (Entity record)
fromPersistValuesThrow entDef doc = 
    case eitherFromPersistValues entDef doc of
        Left s -> Trans.liftIO . throwIO $ PersistMarshalError $ s
        Right entity -> return entity

eitherFromPersistValues :: (PersistEntity record) => EntityDef -> [DB.Field] -> Either T.Text (Entity record)
eitherFromPersistValues entDef doc =
    -- normally the id is the first field: this is probably best even if worst case is worse
    let mKey = lookup _id castDoc
    in case mKey of
         Nothing -> Left "could not find _id field"
         Just key -> case fromPersistValues reorder of
             Right body -> Right $ Entity (Key key) body
             Left e -> Left e
  where
    castDoc = mapFromDoc doc
    castColumns = map (unDBName . fieldDB) $ (entityFields entDef)
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
    -- TODO: the above should be re-thought now that we are no longer inserting null: searching for a null column will look at every returned field before giving up
    -- Also, we are now doing the _id lookup at the start.
    reorder :: [PersistValue] 
    reorder = match castColumns castDoc []
      where
        match :: [T.Text] -> [(T.Text, PersistValue)] -> [PersistValue] -> [PersistValue]
        -- when there are no more Persistent castColumns we are done
        --
        -- allow extra mongoDB fields that persistent does not know about
        -- another application may use fields we don't care about
        -- our own application may set extra fields with the raw driver
        -- TODO: instead use a projection to avoid network overhead
        match [] _ values = values
        match (c:cs) fields values =
          let (found, unused) = matchOne fields []
          in match cs unused (values ++ [snd found])
          where
            matchOne (f:fs) tried =
              if c == fst f then (f, tried ++ fs) else matchOne fs (f:tried)
            -- a Nothing will not be inserted into the document as a null
            -- so if we don't find our column it is a
            -- this keeps the document size down
            matchOne [] tried = ((c, PersistNull), tried)

mapFromDoc :: DB.Document -> [(Text, PersistValue)]
mapFromDoc = Prelude.map (\f -> ( (DB.label f), (fromJust . DB.cast') (DB.value f) ) )

oidToPersistValue :: DB.ObjectId -> PersistValue
oidToPersistValue =  PersistObjectId . Serialize.encode

oidToKey :: (PersistEntity val) => DB.ObjectId -> Key DB.Action val
oidToKey = Key . oidToPersistValue

persistObjectIdToDbOid :: PersistValue -> DB.ObjectId
persistObjectIdToDbOid (PersistObjectId k) = case Serialize.decode k of
                  Left msg -> throw $ PersistError $ T.pack $ "error decoding " ++ (show k) ++ ": " ++ msg
                  Right o -> o
persistObjectIdToDbOid _ = throw $ PersistInvalidField "expected PersistObjectId"

keyToOid :: (PersistEntity val) => Key DB.Action val -> DB.ObjectId
keyToOid (Key k) = persistObjectIdToDbOid k

instance DB.Val PersistValue where
  val (PersistInt64 x)   = DB.Int64 x
  val (PersistText x)    = DB.String x
  val (PersistDouble x)  = DB.Float x
  val (PersistBool x)    = DB.Bool x
  val (PersistUTCTime x) = DB.UTC x
  val (PersistZonedTime (ZT x)) = DB.String $ T.pack $ show x
  val (PersistNull)      = DB.Null
  val (PersistList l)    = DB.Array $ map DB.val l
  val (PersistMap  m)    = DB.Doc $ map (\(k, v)-> (DB.=:) k v) m
  val (PersistByteString x) = DB.Bin (DB.Binary x)
  val x@(PersistObjectId _) = DB.ObjId $ persistObjectIdToDbOid x
  val (PersistDay _)        = throw $ PersistMongoDBUnsupported "only PersistUTCTime currently implemented"
  val (PersistTimeOfDay _)  = throw $ PersistMongoDBUnsupported "only PersistUTCTime currently implemented"
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
  cast' (DB.Doc doc)  = Just $ PersistMap $ mapFromDoc doc
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

dummyFromKey :: Key DB.Action v -> v
dummyFromKey _ = error "dummyFromKey"
dummyFromUnique :: Unique v DB.Action -> v
dummyFromUnique _ = error "dummyFromUnique"
dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"

data MongoAuth = MongoAuth DB.Username DB.Password
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
    }

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
        port               <- o .:? "port" .!= (PortNumber 27017)
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

type instance BackendSpecificFilter DB.Action v = MongoFilter v

data NestedField val nes  =  forall nes1. PersistEntity nes1 => EntityField val nes1  `MidFlds` NestedField nes1 nes
                          | forall nes1. PersistEntity nes1 => EntityField val (Maybe nes1) `MidFldsNullable` NestedField nes1 nes
                          | forall nes1. PersistEntity nes1 => EntityField val nes1 `LastFld` EntityField nes1 nes
                          | forall nes1. PersistEntity nes1 => EntityField val (Maybe nes1) `LastFldNullable` EntityField nes1 nes
data MongoFilter val = forall typ. (PersistField typ) =>
                        NestedFilter {
                          nestedField :: NestedField val typ
                        , fieldValue  :: Either typ [typ]
                        }
                      | forall typ. PersistField typ =>
                        MultiKeyFilter {
                          mulFldKey  :: EntityField val [typ]
                        , mulFldVal  :: Either typ [typ]
                        }
(~>.) :: forall val nes nes1. PersistEntity nes1 => EntityField val nes1 -> NestedField nes1 nes -> NestedField val nes
(~>.)  = MidFlds

(?~>.) :: forall val nes nes1. PersistEntity nes1 => EntityField val (Maybe nes1) -> NestedField nes1 nes -> NestedField val nes
(?~>.) = MidFldsNullable

(->.) :: forall val nes nes1. PersistEntity nes1 => EntityField val nes1 -> EntityField nes1 nes -> NestedField val nes
(->.)  = LastFld

(?->.) :: forall val nes nes1. PersistEntity nes1 => EntityField val (Maybe nes1) -> EntityField nes1 nes -> NestedField val nes
(?->.) = LastFldNullable

infixr 5 ~>.
infixr 5 ?~>.
infixr 6 ->.
infixr 6 ?->.

infixr 4 `nestEq`

-- | use with drill-down operaters ~>, etc
nestEq :: forall v typ. (PersistField typ, PersistEntityBackend v ~ DB.Action) => NestedField v typ -> typ -> Filter v
nf `nestEq` v = BackendFilter $ NestedFilter {nestedField = nf, fieldValue = (Left v)}

-- | use to see if an embedded list contains an item
multiEq :: forall v typ. (PersistField typ, PersistEntityBackend v ~ DB.Action) => EntityField v [typ] -> typ -> Filter v
fld `multiEq` val = BackendFilter $ MultiKeyFilter {mulFldKey = fld, mulFldVal = (Left val)}

mongoFilterToDoc :: PersistEntity val => MongoFilter val -> DB.Document
mongoFilterToDoc (MultiKeyFilter fn v) = return (fieldName fn DB.:= toValue v)
mongoFilterToDoc (NestedFilter fns v) = return ( (nesFldName fns) DB.:= toValue v)
    where
      nesFldName fns' = T.intercalate "." $ nesIdFix . reverse $ nesFldName' fns' []
      nesFldName' :: forall r1 r2. (PersistEntity r1) => NestedField r1 r2 -> [DB.Label] -> [DB.Label]
      nesFldName' ( f1 `MidFlds` f2) lbls = nesFldName' f2 (fieldName f1 : lbls)
      nesFldName' ( f1 `MidFldsNullable` f2) lbls = nesFldName' f2 (fieldName f1:lbls)
      nesFldName' (nf1 `LastFld` nf2) lbls = fieldName nf2:fieldName nf1:lbls
      nesFldName' (nf1 `LastFldNullable` nf2) lbls = fieldName nf2:fieldName nf1:lbls
      nesIdFix [] = []
      nesIdFix (fst':rst') = fst': (map (joinFN . (T.splitOn "_")) rst')
      joinFN :: [Text] -> Text
      joinFN [] = ""
      joinFN (fst':rst') = fst' `T.append` (T.concat (map (\t -> (toUpper . T.head $ t) `T.cons` (T.tail t)) rst'))
