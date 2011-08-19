{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PackageImports, RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.MongoDB
    ( withMongoDBConn
    , withMongoDBPool
    , runMongoDBConn 
    , HostName
    , u
    , DB.Action
    -- , DB.MasterOrSlaveOk(..)
    , DB.AccessMode(..)
    , DB.master
    , DB.slaveOk
    , (DB.=:)
    , ConnectionPool
    , module Database.Persist
    ) where

import Database.Persist
import Database.Persist.Base

import qualified Control.Monad.IO.Class as Trans
import Control.Exception (throw, toException, throwIO)

import qualified Database.MongoDB as DB
import Database.MongoDB.Query (Database)
import Control.Applicative (Applicative)
import Data.UString (u)
import qualified Data.CompactString.UTF8 as CS
import Data.Enumerator hiding (map, length, concatMap, head, replicate)
import Network.Socket (HostName)
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Serialize as Serialize
import Control.Monad.MVar (MonadMVar (..))
import qualified System.IO.Pool as Pool
import Numeric (readHex, showHex)
import Web.PathPieces (SinglePiece (..))

#ifdef DEBUG
import FileLocation (debug, debugMsg)
#else
debug :: forall a. a -> a
debug = id
debugMsg :: forall t a. t -> a -> a
debugMsg _ = id
#endif

type ConnectionPool = (Pool.Pool IOError DB.Pipe, Database)

instance SinglePiece (Key DB.Action entity) where
    toSinglePiece (Key pOid@(PersistObjectId _)) =
        let (DB.Oid w32 w64) = persistObjectIdToDbOid $ debugMsg "POid" pOid
        in  debug $ T.pack $ showHexLen 8 w32 ++ showHexLen 16 w64
    toSinglePiece k = throw $ PersistInvalidField $ "Invalid Key: " ++ show k

    fromSinglePiece str =
      case (readHex $ (T.unpack $ debug str))::[(Int,String)] of
        [] -> Nothing
        parsed -> Just $ Key $ PersistObjectId $ Serialize.encode . fst $ head $ debug parsed

showHexLen :: (Integral a) => Int -> a -> String
showHexLen n x = let s = showHex x ""
                 in replicate (n - length s) '0' ++ s

{-
lexObjectIdP :: ReadP.ReadP ObjectId
lexObjectIdP = do
  ReadP.skipSpaces
  digits1 <- fmap fromHex $ ReadP.count 8  hexDigit :: ReadP.ReadP Word32
  digits2 <- fmap fromHex $ ReadP.count 16 hexDigit :: ReadP.ReadP Word64
  return $ ObjectId (digits1, digits2)
  where
    hexDigit = ReadP.satisfy isHexDigit
    
    fromHex :: (Integral a) => String -> a
    fromHex = foldl (\x -> (x * 16 +) . fromIntegral . digitToInt) 0
-}

withMongoDBConn :: (Trans.MonadIO m, Applicative m) =>
  Database -> HostName -> (ConnectionPool -> m b) -> m b
withMongoDBConn dbname hostname = withMongoDBPool dbname hostname 1

withMongoDBPool :: (Trans.MonadIO m, Applicative m) =>
  Database -> HostName -> Int -> (ConnectionPool -> m b) -> m b
withMongoDBPool dbname hostname connectionPoolSize connectionReader = do
  --pool <- runReaderT (DB.newConnPool connectionPoolSize $ DB.host hostname) $ ANetwork Internet
  pool <- Trans.liftIO $ Pool.newPool Pool.Factory { Pool.newResource  = DB.connect (DB.host hostname)
                                                  , Pool.killResource = DB.close
                                                  , Pool.isExpired    = DB.isClosed
                                                  }
                                     connectionPoolSize
  connectionReader (pool, dbname)

runMongoDBConn :: (Trans.MonadIO m) => DB.AccessMode  ->  DB.Action m b -> ConnectionPool -> m b
runMongoDBConn accessMode action (pool, databaseName) = do
  pipe <- Trans.liftIO $ DB.runIOE $ Pool.aResource pool
  res  <- DB.access pipe accessMode databaseName action
  either (Trans.liftIO . throwIO . PersistMongoDBError . show) return res

value :: DB.Field -> DB.Value
value (_ DB.:= val) = val

rightPersistVals :: (PersistEntity val) => EntityDef -> [DB.Field] -> val
rightPersistVals ent vals = case wrapFromPersistValues ent vals of
      Left e -> error e
      Right v -> v

filterByKey :: (PersistEntity val) => Key DB.Action val -> DB.Document
filterByKey k = [u"_id" DB.=: keyToDbOid k]

queryByKey :: (PersistEntity val) => Key DB.Action val -> EntityDef -> DB.Query 
queryByKey k entity = (DB.select (filterByKey k) (u $ entityName entity)) 

selectByKey :: (PersistEntity val) => Key DB.Action val -> EntityDef -> DB.Selection 
selectByKey k entity = (DB.select (filterByKey k) (u $ entityName entity))

updateFields :: (PersistEntity val) => [Update val] -> [DB.Field]
updateFields upds = map updateToMongoField upds 

updateToMongoField :: (PersistEntity val) => Update val -> DB.Field
updateToMongoField upd@(Update _ v up) = opName DB.:= DB.Doc [( (u $ updateFieldName upd) DB.:= opValue)]
    where 
      opValue = DB.val . snd $ opNameValue
      opName = fst opNameValue
      opNameValue =
        case (up, toPersistValue v) of
                  (Assign,a)    -> (u "$set", a)
                  (Add, a)      -> (u "$inc", a)
                  (Subtract, PersistInt64 i) -> (u "$inc", PersistInt64 (-i))
                  (Subtract, _) -> error "expected PersistInt64 for a subtraction"
                  (Multiply, _) -> throw $ PersistMongoDBUnsupported "multiply not supported"
                  (Divide, _)   -> throw $ PersistMongoDBUnsupported "divide not supported"


uniqSelector :: forall val.  (PersistEntity val) => Unique val DB.Action -> [DB.Field]
uniqSelector uniq = zipWith (DB.:=)
  (map u (persistUniqueToFieldNames uniq))
  (map DB.val (persistUniqueToValues uniq))

pairFromDocument :: forall val val1.  (PersistEntity val, PersistEntity val1) => EntityDef -> [DB.Field] -> Either String (Key DB.Action val, val1)
pairFromDocument ent document = pairFromPersistValues document
  where
    pairFromPersistValues (x:xs) =
        case wrapFromPersistValues ent xs of
            Left e -> Left e
            Right xs' -> Right ((Key . dbOidToKey . fromJust . DB.cast' . value) x, xs')
    pairFromPersistValues _ = Left "error in fromPersistValues'"

insertFields :: forall val.  (PersistEntity val) => EntityDef -> val -> [DB.Field]
insertFields t record = zipWith (DB.:=) (toLabels) (toValues)
  where
    toLabels = map (u . columnName) $ entityColumns t
    toValues = map (DB.val . toPersistValue) (toPersistFields record)

instance (Trans.MonadIO m, Applicative m, Functor m, MonadMVar m) => PersistBackend DB.Action m where
    insert record = do
        (DB.ObjId oid) <- DB.insert (u $ entityName t) (insertFields t record)
        return $ Key $ dbOidToKey oid 
      where
        t = entityDef record

    replace k record = do
        DB.replace (selectByKey k t) (insertFields t record)
        return ()
      where
        t = entityDef record

    update _ [] = return ()
    update k upds =
        DB.modify 
           (DB.Select [u"_id" DB.:= (DB.ObjId $ keyToDbOid k)]  (u $ entityName t)) 
           $ updateFields upds
      where
        t = entityDef $ dummyFromKey k

    updateWhere _ [] = return ()
    updateWhere filts upds =
        DB.modify DB.Select {
          DB.coll = (u $ entityName t)
        , DB.selector = filtersToSelector filts
        } $ updateFields upds
      where
        t = entityDef $ dummyFromFilts filts

    delete k =
        DB.deleteOne DB.Select {
          DB.coll = (u $ entityName t)
        , DB.selector = filterByKey k
        }
      where
        t = entityDef $ dummyFromKey k

    deleteWhere filts = do
        DB.delete DB.Select {
          DB.coll = (u $ entityName t)
        , DB.selector = filtersToSelector filts
        }
      where
        t = entityDef $ dummyFromFilts filts

    deleteBy uniq =
        DB.delete DB.Select {
          DB.coll = u $ entityName t
        , DB.selector = uniqSelector uniq
        }
      where
        t = entityDef $ dummyFromUnique uniq

    get k = do
            d <- DB.findOne (queryByKey k t)
            case d of
              Nothing -> return Nothing
              Just doc -> do
                return $ Just $ rightPersistVals t (tail doc)
          where
            t = entityDef $ dummyFromKey k

    getBy uniq = do
        mdocument <- DB.findOne $
          (DB.select (uniqSelector uniq) (u $ entityName t))
        case mdocument of
          Nothing -> return Nothing
          Just document -> case pairFromDocument t document of
              Left s -> Trans.liftIO . throwIO $ PersistMarshalError s
              Right (k, x) -> return $ Just (k, x)
      where
        t = entityDef $ dummyFromUnique uniq

    count filts = do
        i <- DB.count query
        return $ fromIntegral i
      where
        query = DB.select (filtersToSelector filts) (u $ entityName t)
        t = entityDef $ dummyFromFilts filts

    selectEnum filts opts = Iteratee . start
      where
        start x = do
            cursor <- DB.find $ makeQuery filts opts
            loop x cursor

        t = entityDef $ dummyFromFilts filts

        loop (Continue k) curs = do
            doc <- DB.next curs
            case doc of
                Nothing -> return $ Continue k
                Just document -> case pairFromDocument t document of
                        Left s -> return $ Error $ toException
                                    $ PersistMarshalError s
                        Right row -> do
                            step <- runIteratee $ k $ Chunks [row]
                            loop step curs
        loop step _ = return step

    selectFirst filts opts = do
        doc <- DB.findOne $ makeQuery filts opts
        case doc of
            Nothing -> return Nothing
            Just document -> case pairFromDocument t document of
                Left s -> Trans.liftIO . throwIO $ PersistMarshalError s
                Right row -> return $ Just row
      where
        t = entityDef $ dummyFromFilts filts

    selectKeys filts =
        Iteratee . start
      where
        start x = do
            cursor <- DB.find query
            loop x cursor

        loop (Continue k) curs = do
            doc <- DB.next curs
            case doc of
                Nothing -> return $ Continue k
                Just [_ DB.:= (DB.ObjId oid)] -> do
                    step <- runIteratee $ k $ Chunks [Key $ dbOidToKey oid]
                    loop step curs
                Just y -> return $ Error $ toException $ PersistMarshalError
                        $ "Unexpected in selectKeys: " ++ show y
        loop step _ = return step

        query = (DB.select (filtersToSelector filts) (u $ entityName t)) {
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
    (DB.select (filtersToSelector filts) (u $ entityName t)) {
      DB.limit = fromIntegral limit
    , DB.skip  = fromIntegral offset
    , DB.sort  = orders
    }
  where
    t = entityDef $ dummyFromFilts filts
    limit  = fst3 $ limitOffsetOrder opts
    offset = snd3 $ limitOffsetOrder opts
    orders = map orderClause $ third3 $ limitOffsetOrder opts

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
      FilterOr fs  -> multiFilter "$or" fs
      -- I didn't even know about the $and operator.
      -- It is unecessary in 99% of cases.
      -- However it makes query construction easier in special cases
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
    showFilter (BackendSpecificFilter bsf) = throw $ PersistMongoDBError $ "did not expect BackendSpecificFilter " ++ bsf

fieldName ::  forall v typ.  (PersistEntity v) => Field v typ -> CS.CompactString
fieldName = u . idfix . columnName . persistColumnDef
  where idfix f = if f == "id" then "_id" else f


wrapFromPersistValues :: (PersistEntity val) => EntityDef -> [DB.Field] -> Either String val
wrapFromPersistValues e doc = fromPersistValues reorder
  where
    castDoc = mapFromDoc doc
    castColumns = map (T.pack . columnName) $ (entityColumns e)
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
    reorder :: [PersistValue] 
    reorder = match castColumns castDoc []
      where
        match :: [T.Text] -> [(T.Text, PersistValue)] -> [PersistValue] -> [PersistValue]
        match [] [] values = values
        match (c:cs) fields values =
          let (found, unused) = matchOne fields []
          in match cs unused (values ++ [snd found])
          where
            matchOne (f:fs) tried =
              if c == fst f then (f, tried ++ fs) else matchOne fs (f:tried)
            matchOne fs tried = throw $ PersistError $ "reorder error: field doesn't match" ++ (show c) ++ (show fs) ++ (show tried)
        match cs fs values = throw $ PersistError $ "reorder error: fields don't match" ++ (show cs) ++ (show fs) ++ (show values)

mapFromDoc :: DB.Document -> [(T.Text, PersistValue)]
mapFromDoc = Prelude.map (\f -> ( ( csToT (DB.label f)), (fromJust . DB.cast') (DB.value f) ) )

csToT :: CS.CompactString -> T.Text
csToT = E.decodeUtf8 . CS.toByteString

tToCS :: T.Text -> CS.CompactString
tToCS = CS.fromByteString_ . E.encodeUtf8

dbOidToKey :: DB.ObjectId -> PersistValue
dbOidToKey =  PersistObjectId . Serialize.encode

persistObjectIdToDbOid :: PersistValue -> DB.ObjectId
persistObjectIdToDbOid (PersistObjectId k) = case Serialize.decode k of
                  Left msg -> throw $ PersistError $ "error decoding " ++ (show k) ++ ": " ++ msg
                  Right o -> o
persistObjectIdToDbOid _ = throw $ PersistInvalidField "expected PersistObjectId"

keyToDbOid :: (PersistEntity val) => Key DB.Action val -> DB.ObjectId
keyToDbOid (Key k) = persistObjectIdToDbOid k

instance DB.Val PersistValue where
  val (PersistInt64 x)   = DB.Int64 x
  val (PersistText x)    = DB.String (tToCS x)
  val (PersistDouble x)  = DB.Float x
  val (PersistBool x)    = DB.Bool x
  val (PersistUTCTime x) = DB.UTC x
  val (PersistNull)      = DB.Null
  val (PersistList l)    = DB.Array $ map DB.val l
  val (PersistMap  m)    = DB.Doc $ map (\(k, v)-> (DB.=:) (tToCS k) v) m
  val (PersistByteString x) = DB.String $ CS.fromByteString_ x 
  val x@(PersistObjectId _) = DB.ObjId $ persistObjectIdToDbOid x
  val (PersistDay _)        = throw $ PersistMongoDBUnsupported "only PersistUTCTime currently implemented"
  val (PersistTimeOfDay _)  = throw $ PersistMongoDBUnsupported "only PersistUTCTime currently implemented"
  cast' (DB.Float x)  = Just (PersistDouble x)
  cast' (DB.Int32 x)  = Just $ PersistInt64 $ fromIntegral x
  cast' (DB.Int64 x)  = Just $ PersistInt64 x
  cast' (DB.String x) = Just $ PersistText (csToT x) 
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
  cast' (DB.ObjId x)  = Just $ dbOidToKey x 
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
