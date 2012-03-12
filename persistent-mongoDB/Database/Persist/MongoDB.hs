{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PackageImports, RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.MongoDB
    (
    -- * using connections
      withMongoDBConn
    , withMongoDBPool
    , createMongoDBPool
    , runMongoDBConn 
    , ConnectionPool
    , MongoConf (..)
    -- * Key conversion helpers
    , keyToOid
    , oidToKey
    -- * CompactString helpers
    , csToText
    , textToCS
    -- * network type
    , HostName
    -- * UString type
    , u
    -- * MongoDB driver types
    , DB.Action
    , DB.AccessMode(..)
    , DB.master
    , DB.slaveOk
    , (DB.=:)
    -- * Database.Persistent
    , module Database.Persist
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
import Data.UString (u)
import qualified Data.CompactString.UTF8 as CS
import Network.Socket (HostName)
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Serialize as Serialize
import qualified System.IO.Pool as Pool
import Web.PathPieces (PathPiece (..))
import qualified Data.Conduit as C
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Object), (.:), (.:?), (.!=))
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadThrow (..))

#ifdef DEBUG
import FileLocation (debug)
#else
{-
debug :: forall a. a -> a
debug = id
debugMsg :: forall t a. t -> a -> a
debugMsg _ = id
-}
#endif

type ConnectionPool = (Pool.Pool IOError DB.Pipe, Database)

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
  Database -> HostName -> (ConnectionPool -> m b) -> m b
withMongoDBConn dbname hostname = withMongoDBPool dbname hostname 1

createMongoDBPool :: (Trans.MonadIO m, Applicative m) =>
  Database -> HostName -> Int -> m ConnectionPool
createMongoDBPool dbname hostname connectionPoolSize = do
  --pool <- runReaderT (DB.newConnPool connectionPoolSize $ DB.host hostname) $ ANetwork Internet
  pool <- Trans.liftIO $ Pool.newPool Pool.Factory { Pool.newResource  = DB.connect (DB.host hostname)
                                                  , Pool.killResource = DB.close
                                                  , Pool.isExpired    = DB.isClosed
                                                  }
                                     connectionPoolSize
  return (pool, dbname)

withMongoDBPool :: (Trans.MonadIO m, Applicative m) =>
  Database -> HostName -> Int -> (ConnectionPool -> m b) -> m b
withMongoDBPool dbname hostname connectionPoolSize connectionReader = do
  pool <- createMongoDBPool dbname hostname connectionPoolSize
  connectionReader pool

runMongoDBConn :: (Trans.MonadIO m) => DB.AccessMode  ->  DB.Action m b -> ConnectionPool -> m b
runMongoDBConn accessMode action (pool, databaseName) = do
  pipe <- Trans.liftIO $ DB.runIOE $ Pool.aResource pool
  res  <- DB.access pipe accessMode databaseName action
  either (Trans.liftIO . throwIO . PersistMongoDBError . T.pack . show) return res

value :: DB.Field -> DB.Value
value (_ DB.:= val) = val

rightPersistVals :: (PersistEntity val) => EntityDef -> [DB.Field] -> val
rightPersistVals ent vals = case wrapFromPersistValues ent vals of
      Left e -> error $ T.unpack e
      Right v -> v

filterByKey :: (PersistEntity val) => Key DB.Action val -> DB.Document
filterByKey k = [u"_id" DB.=: keyToOid k]

queryByKey :: (PersistEntity val) => Key DB.Action val -> EntityDef -> DB.Query 
queryByKey k entity = (DB.select (filterByKey k) (u $ T.unpack $ unDBName $ entityDB entity)) 

selectByKey :: (PersistEntity val) => Key DB.Action val -> EntityDef -> DB.Selection 
selectByKey k entity = (DB.select (filterByKey k) (u $ T.unpack $ unDBName $ entityDB entity))

updateFields :: (PersistEntity val) => [Update val] -> [DB.Field]
updateFields upds = map updateToMongoField upds 

updateToMongoField :: (PersistEntity val) => Update val -> DB.Field
updateToMongoField (Update field v up) =
    opName DB.:= DB.Doc [( (u $ T.unpack $ unDBName $ fieldDB $ persistFieldDef field) DB.:= opValue)]
    where 
      (opName, opValue) =
        case (up, toPersistValue v) of
                  (Assign, PersistNull) -> (u"$unset", DB.Int64 1)
                  (Assign,a)    -> (u"$set", DB.val a)
                  (Add, a)      -> (u"$inc", DB.val a)
                  (Subtract, PersistInt64 i) -> (u "$inc", DB.Int64 (-i))
                  (Subtract, _) -> error "expected PersistInt64 for a subtraction"
                  (Multiply, _) -> throw $ PersistMongoDBUnsupported "multiply not supported"
                  (Divide, _)   -> throw $ PersistMongoDBUnsupported "divide not supported"


uniqSelector :: forall val.  (PersistEntity val) => Unique val DB.Action -> [DB.Field]
uniqSelector uniq = zipWith (DB.:=)
  (map u (map (T.unpack . unDBName . snd) $ persistUniqueToFieldNames uniq))
  (map DB.val (persistUniqueToValues uniq))

pairFromDocument :: (PersistEntity val, PersistEntityBackend val ~ DB.Action)
                 => EntityDef
                 -> [DB.Field]
                 -> Either String (Entity val)
pairFromDocument ent document = pairFromPersistValues document
  where
    pairFromPersistValues (x:xs) =
        case wrapFromPersistValues ent xs of
            Left e -> Left $ T.unpack e
            Right xs' -> Right (Entity (oidToKey . fromJust . DB.cast' . value $ x) xs')
    pairFromPersistValues _ = Left "error in fromPersistValues'"

insertFields :: forall val.  (PersistEntity val) => EntityDef -> val -> [DB.Field]
insertFields t record = zipFilter (entityFields t) (toPersistFields record)
  where
    zipFilter [] _  = []
    zipFilter _  [] = []
    zipFilter (e:efields) (p:pfields) = let pv = toPersistValue p in
        if pv == PersistNull then zipFilter efields pfields
          else (toLabel e DB.:= DB.val pv):zipFilter efields pfields

    toLabel = u . T.unpack . unDBName . fieldDB

saveWithKey :: forall m ent record. (Applicative m, Functor m, MonadBaseControl IO m, PersistEntity ent, PersistEntity record)
            => (DB.Collection -> DB.Document -> DB.Action m () )
            -> Key DB.Action ent -> record -> DB.Action m ()
saveWithKey dbSave k record =
      dbSave (u $ T.unpack $ unDBName $ entityDB t) ((persistKeyToMongoId k):(insertFields t record))
    where
      t = entityDef record

instance (Applicative m, Functor m, Trans.MonadIO m, MonadBaseControl IO m) => PersistStore DB.Action m where
    insert record = do
        (DB.ObjId oid) <- DB.insert (u $ T.unpack $ unDBName $ entityDB t) (insertFields t record)
        return $ oidToKey oid 
      where
        t = entityDef record

    insertKey k record = saveWithKey DB.insert_ k record

    repsert   k record = saveWithKey DB.save k record

    replace k record = do
        DB.replace (selectByKey k t) (insertFields t record)
        return ()
      where
        t = entityDef record

    delete k =
        DB.deleteOne DB.Select {
          DB.coll = (u $ T.unpack $ unDBName $ entityDB t)
        , DB.selector = filterByKey k
        }
      where
        t = entityDef $ dummyFromKey k

    get k = do
            d <- DB.findOne (queryByKey k t)
            case d of
              Nothing -> return Nothing
              Just doc -> do
                return $ Just $ rightPersistVals t (tail doc)
          where
            t = entityDef $ dummyFromKey k

instance MonadThrow m => MonadThrow (DB.Action m) where
    monadThrow = lift . monadThrow

instance (Applicative m, Functor m, Trans.MonadIO m, MonadBaseControl IO m) => PersistUnique DB.Action m where
    getBy uniq = do
        mdocument <- DB.findOne $
          (DB.select (uniqSelector uniq) (u $ T.unpack $ unDBName $ entityDB t))
        case mdocument of
          Nothing -> return Nothing
          Just document -> case pairFromDocument t document of
              Left s -> Trans.liftIO . throwIO $ PersistMarshalError $ T.pack s
              Right e -> return $ Just e
      where
        t = entityDef $ dummyFromUnique uniq

    deleteBy uniq =
        DB.delete DB.Select {
          DB.coll = u $ T.unpack $ unDBName $ entityDB t
        , DB.selector = uniqSelector uniq
        }
      where
        t = entityDef $ dummyFromUnique uniq

persistKeyToMongoId :: PersistEntity val => Key DB.Action val -> DB.Field
persistKeyToMongoId k = u"_id" DB.:= (DB.ObjId $ keyToOid k)

instance (Applicative m, Functor m, Trans.MonadIO m, MonadBaseControl IO m) => PersistQuery DB.Action m where
    update _ [] = return ()
    update k upds =
        DB.modify 
           (DB.Select [persistKeyToMongoId k]  (u $ T.unpack $ unDBName $ entityDB t)) 
           $ updateFields upds
      where
        t = entityDef $ dummyFromKey k

    updateWhere _ [] = return ()
    updateWhere filts upds =
        DB.modify DB.Select {
          DB.coll = (u $ T.unpack $ unDBName $ entityDB t)
        , DB.selector = filtersToSelector filts
        } $ updateFields upds
      where
        t = entityDef $ dummyFromFilts filts

    deleteWhere filts = do
        DB.delete DB.Select {
          DB.coll = (u $ T.unpack $ unDBName $ entityDB t)
        , DB.selector = filtersToSelector filts
        }
      where
        t = entityDef $ dummyFromFilts filts

    count filts = do
        i <- DB.count query
        return $ fromIntegral i
      where
        query = DB.select (filtersToSelector filts) (u $ T.unpack $ unDBName $ entityDB t)
        t = entityDef $ dummyFromFilts filts

    selectSource filts opts = C.SourceM
        (do
            cursor <- lift $ DB.find $ makeQuery filts opts
            return $ mkSrc cursor)
        (return ())
      where
        mkSrc cursor = C.SourceM (pull cursor) (return ())
        pull cursor = lift $ do
            mdoc <- DB.next cursor
            case mdoc of
                Nothing -> return C.Closed
                Just doc ->
                    case pairFromDocument t doc of
                        Left s -> liftIO $ throwIO $ PersistMarshalError $ T.pack s
                        Right row -> return $ C.Open (mkSrc cursor) (return ()) row
        t = entityDef $ dummyFromFilts filts

    selectFirst filts opts = do
        doc <- DB.findOne $ makeQuery filts opts
        case doc of
            Nothing -> return Nothing
            Just document -> case pairFromDocument t document of
                Left s -> Trans.liftIO . throwIO $ PersistMarshalError $ T.pack s
                Right row -> return $ Just row
      where
        t = entityDef $ dummyFromFilts filts

    selectKeys filts = C.SourceM
        (do
            cursor <- lift $ DB.find query
            return $ mkSrc cursor)
        (return ())
      where
        mkSrc cursor = C.SourceM (pull cursor) (return ())
        pull cursor = lift $ do
            mdoc <- DB.next cursor
            case mdoc of
                Nothing -> return C.Closed
                Just [_ DB.:= DB.ObjId oid] -> return $ C.Open (mkSrc cursor) (return ()) $ oidToKey oid
                Just y -> liftIO $ throwIO $ PersistMarshalError $ T.pack $ "Unexpected in selectKeys: " ++ show y
        query = (DB.select (filtersToSelector filts) (u $ T.unpack $ unDBName $ entityDB t)) {
          DB.project = [u"_id" DB.=: (1 :: Int)]
        }
        t = entityDef $ dummyFromFilts filts

orderClause :: PersistEntity val => SelectOpt val -> DB.Field
orderClause o = case o of
                  Asc f  -> fieldName f DB.=: ( 1 :: Int)
                  Desc f -> fieldName f DB.=: (-1 :: Int)
                  _      -> error "orderClause: expected Asc or Desc"


makeQuery :: PersistEntity val => [Filter val] -> [SelectOpt val] -> DB.Query
makeQuery filts opts =
    (DB.select (filtersToSelector filts) (u $ T.unpack $ unDBName $ entityDB t)) {
      DB.limit = fromIntegral limit
    , DB.skip  = fromIntegral offset
    , DB.sort  = orders
    }
  where
    t = entityDef $ dummyFromFilts filts
    (limit, offset, orders') = limitOffsetOrder opts
    orders = map orderClause orders'

filtersToSelector :: PersistEntity val => [Filter val] -> DB.Document
filtersToSelector filts = 
#ifdef DEBUG
  debug $
#endif
    if null filts then [] else concatMap filterToDocument filts

multiFilter :: forall val.  PersistEntity val => String -> [Filter val] -> [DB.Field]
multiFilter multi fs = [u multi  DB.:= DB.Array (map (DB.Doc . filterToDocument) fs)]

filterToDocument :: PersistEntity val => Filter val -> DB.Document
filterToDocument f =
    case f of
      Filter field v filt -> return $ case filt of
          Eq -> fieldName field DB.:= toValue v
          _  -> fieldName field DB.=: [u(showFilter filt) DB.:= toValue v]
      FilterOr [] -> -- Michael decided to follow Haskell's semantics, which seems reasonable to me.
                     -- in Haskell an empty or is a False
                     -- Perhaps there is a less hacky way of creating a query that always returns false?
                     [u"$not" DB.=: [u"$exists" DB.=: u"_id"]]
      FilterOr fs  -> multiFilter "$or" fs
      -- $and is usually unecessary but makes query construction easier in special cases
      FilterAnd [] -> []
      FilterAnd fs -> multiFilter "$and" fs
  where
    toValue :: forall a.  PersistField a => Either a [a] -> DB.Value
    toValue val =
      case val of
        Left v   -> DB.val $ toPersistValue v
        Right vs -> DB.val $ map toPersistValue vs

    showFilter Ne = "$ne"
    showFilter Gt = "$gt"
    showFilter Lt = "$lt"
    showFilter Ge = "$gte"
    showFilter Le = "$lte"
    showFilter In = "$in"
    showFilter NotIn = "$nin"
    showFilter Eq = error "EQ filter not expected"
    showFilter (BackendSpecificFilter bsf) = throw $ PersistMongoDBError $ T.pack $ "did not expect BackendSpecificFilter " ++ T.unpack bsf

fieldName ::  forall v typ.  (PersistEntity v) => EntityField v typ -> CS.CompactString
fieldName = u . idfix . T.unpack . unDBName . fieldDB . persistFieldDef
  where idfix f = if f == "id" then "_id" else f


wrapFromPersistValues :: (PersistEntity val) => EntityDef -> [DB.Field] -> Either T.Text val
wrapFromPersistValues e doc = fromPersistValues reorder
  where
    castDoc = mapFromDoc doc
    castColumns = map (unDBName . fieldDB) $ (entityFields e)
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

mapFromDoc :: DB.Document -> [(T.Text, PersistValue)]
mapFromDoc = Prelude.map (\f -> ( ( csToText (DB.label f)), (fromJust . DB.cast') (DB.value f) ) )

-- | CompactString is UTF8, Text is UTF16
csToText :: CS.CompactString -> T.Text
csToText = E.decodeUtf8 . CS.toByteString

-- | CompactString is UTF8, Text is UTF16
textToCS :: T.Text -> CS.CompactString
textToCS = CS.fromByteString_ . E.encodeUtf8

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
  val (PersistText x)    = DB.String (textToCS x)
  val (PersistDouble x)  = DB.Float x
  val (PersistBool x)    = DB.Bool x
  val (PersistUTCTime x) = DB.UTC x
  val (PersistNull)      = DB.Null
  val (PersistList l)    = DB.Array $ map DB.val l
  val (PersistMap  m)    = DB.Doc $ map (\(k, v)-> (DB.=:) (textToCS k) v) m
  val (PersistByteString x) = DB.String $ CS.fromByteString_ x 
  val x@(PersistObjectId _) = DB.ObjId $ persistObjectIdToDbOid x
  val (PersistDay _)        = throw $ PersistMongoDBUnsupported "only PersistUTCTime currently implemented"
  val (PersistTimeOfDay _)  = throw $ PersistMongoDBUnsupported "only PersistUTCTime currently implemented"
  cast' (DB.Float x)  = Just (PersistDouble x)
  cast' (DB.Int32 x)  = Just $ PersistInt64 $ fromIntegral x
  cast' (DB.Int64 x)  = Just $ PersistInt64 x
  cast' (DB.String x) = Just $ PersistText (csToText x)
  cast' (DB.Bool x)   = Just $ PersistBool x
  cast' (DB.UTC d)    = Just $ PersistUTCTime d
  cast' DB.Null       = Just $ PersistNull
  cast' (DB.Bin (DB.Binary b))   = Just $ PersistByteString b
  cast' (DB.Fun (DB.Function f)) = Just $ PersistByteString f
  cast' (DB.Uuid (DB.UUID uid))  = Just $ PersistByteString uid
  cast' (DB.Md5 (DB.MD5 md5))    = Just $ PersistByteString md5
  cast' (DB.UserDef (DB.UserDefined bs)) = Just $ PersistByteString bs
  cast' (DB.RegEx (DB.Regex us1 us2))    = Just $ PersistByteString $ CS.toByteString $ CS.append us1 us2
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

-- | Information required to connect to a mongo database
data MongoConf = MongoConf
    { mgDatabase :: String
    , mgHost     :: String
    , mgPoolSize :: Int
    , mgAccessMode :: DB.AccessMode
    }

instance PersistConfig MongoConf where
    type PersistConfigBackend MongoConf = DB.Action
    type PersistConfigPool MongoConf = ConnectionPool
    createPoolConfig (MongoConf db host poolsize _) = createMongoDBPool (u db) host poolsize
    runPool (MongoConf _ _ _ accessMode) = runMongoDBConn accessMode
    loadConfig (Object o) = do
        db    <- o .: "database"
        host  <- o .: "host"
        pool  <- o .: "poolsize"
        accessString <- o .:? "accessMode" .!= "ConfirmWrites"

        accessMode <- case accessString of
               "ReadStaleOk"       -> return DB.ReadStaleOk
               "UnconfirmedWrites" -> return DB.UnconfirmedWrites
               "ConfirmWrites"     -> return $ DB.ConfirmWrites [u"j" DB.=: True]
               badAccess -> fail $ "unknown accessMode: " ++ (T.unpack badAccess)

        return $ MongoConf (T.unpack db) (T.unpack host) pool accessMode
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
