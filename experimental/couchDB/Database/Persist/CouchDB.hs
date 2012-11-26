{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Persist.CouchDB
    ( withCouchDBConn
    , withCouchDBPool
    , runCouchDBConn
    , ConnectionPool
    , module Database.Persist
    , CouchConf (..)
    ) where

import Database.Persist
import Database.Persist.Base

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Trans.Class (MonadTrans (..))
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Control.Monad.Trans.Control (MonadBaseControl (..), ComposeSt, defaultLiftBaseWith, defaultRestoreM, MonadTransControl (..))
import Control.Applicative (Applicative)

import Text.JSON
import Data.Char
import Data.List (intercalate, nub, nubBy)
import Data.Pool
import Data.Maybe
import Data.Object
import Data.Digest.Pure.SHA
import Data.Neither (MEither (..), meither)
import Data.Enumerator (Stream (..), Step (..), Iteratee (..), returnI, run_, ($$))
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Enumerator.List as EL
import qualified Database.CouchDB as DB
import qualified Control.Exception.Base as E

type Couch = (DB.CouchConn, DB.DB)
type ConnectionPool = Pool Couch

newtype (CouchReader m a) = CouchReader {unCouchConn :: ReaderT Couch m a}
    deriving (Monad, MonadIO, MonadTrans, MonadCatchIO, Functor, Applicative)

instance (MonadBase b m) => MonadBase b (CouchReader m) where
    liftBase = lift . liftBase

instance (MonadBaseControl b m) => MonadBaseControl b (CouchReader m) where
     newtype StM (CouchReader m) a = StMSP {unStMSP :: ComposeSt CouchReader m a}
     liftBaseWith = defaultLiftBaseWith StMSP
     restoreM = defaultRestoreM unStMSP

instance MonadTransControl CouchReader where
    newtype StT CouchReader a = StReader {unStReader :: a}
    liftWith f = CouchReader . ReaderT $ \r -> f $ \t -> liftM StReader $ runReaderT (unCouchConn t) r
    restoreT = CouchReader . ReaderT . const . liftM unStReader

-- | Open one database connection.
withCouchDBConn :: (MonadBaseControl IO m, MonadIO m)
    => String -- ^ database name
    -> String -- ^ host name (typically \"localhost\")
    -> Int    -- ^ port number (typically 5984)
    -> (ConnectionPool -> m b) -> m b
withCouchDBConn db host port = withCouchDBPool db host port 1

-- | Open one or more database connections.
withCouchDBPool :: (MonadBaseControl IO m, MonadIO m)
    => String -- ^ database name
    -> String -- ^ host name (typically \"localhost\")
    -> Int    -- ^ port number (typically 5984)
    -> Int    -- ^ number of connections to open
    -> (ConnectionPool -> m b) -> m b
withCouchDBPool db host port = createPool
    (do unless (DB.isDBString db)
               (error "Wrong database name.")
        conn <- DB.createCouchConn host port
        E.catch (run conn $ DB.createDB db)
                (\(E.ErrorCall _) -> return ())
        return (conn, DB.db db))
    (DB.closeCouchConn . fst)

-- | Run the database connection. (Typical usage: withCouchDBConn \"database\" \"localhost\" 5984 $ runCouchDBConn $ do ...)
runCouchDBConn :: (MonadBaseControl IO m, MonadIO m) => CouchReader m a -> ConnectionPool -> m a
runCouchDBConn (CouchReader r) pconn = withPool' pconn $ runReaderT r

run :: (MonadIO m) => DB.CouchConn -> DB.CouchMonad a -> m a
run conn x = liftIO . DB.runCouchDBWith conn $ x

docToKey :: DB.Doc -> Key backend entity
docToKey = Key . PersistText . T.pack . show

keyToDoc :: Key backend entity -> DB.Doc
keyToDoc (Key (PersistText x)) = DB.doc $ T.unpack x

fromResult :: Result a -> a
fromResult x = case resultToEither x of
                    Right r -> r
                    Left l -> error l

instance JSON PersistValue where
    readJSON (JSNull) = Ok $ PersistNull
    readJSON (JSBool x) = Ok $ PersistBool x
    readJSON (JSRational False x) = Ok . PersistInt64 $ truncate x
    readJSON (JSRational True x) =  Ok . PersistDouble $ fromRational x
    readJSON (JSString x) = Ok . PersistText . T.pack $ fromJSString x
    readJSON (JSArray x) = Ok . PersistList $ map (fromResult . readJSON) x
    readJSON (JSObject x) = Ok . PersistMap . map (\(k, v) -> (T.pack k, fromResult $ readJSON v)) $ fromJSObject x
    showJSON (PersistText x) = JSString . toJSString $ T.unpack x
    showJSON (PersistByteString x) = JSString . toJSString $ B.unpack x
    showJSON (PersistInt64 x) = JSRational False $ fromIntegral x
    showJSON (PersistDouble x) = JSRational True $ toRational x
    showJSON (PersistBool x) = JSBool x
    showJSON (PersistDay x) = JSString . toJSString $ show x
    showJSON (PersistTimeOfDay x) = JSString . toJSString $ show x
    showJSON (PersistUTCTime x) = JSString . toJSString $ show x
    showJSON (PersistNull) = JSNull
    showJSON (PersistList x) = JSArray $ map showJSON x
    showJSON (PersistMap x) = JSObject . toJSObject $ map (\(k, v) -> (T.unpack k, showJSON v)) x
    showJSON (PersistObjectId _) = error "PersistObjectId is not supported."

entityToJSON :: (PersistEntity val) => val -> JSValue
entityToJSON x = JSObject . toJSObject $ zip names values
    where names = map columnName . entityColumns $ entityDef x
          values = map (showJSON . toPersistValue) $ toPersistFields x

uniqueToJSON :: [PersistValue] -> JSValue
uniqueToJSON [] = JSNull
uniqueToJSON [x] = showJSON x
uniqueToJSON xs = JSArray $ map showJSON xs

dummyFromKey :: Key backend v -> v
dummyFromKey _ = error "dummyFromKey"

dummyFromUnique :: Unique v backend -> v
dummyFromUnique _ = error "dummyFromUnique"

dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"

wrapFromPersistValues :: (PersistEntity val) => EntityDef -> PersistValue -> Either String val
wrapFromPersistValues e doc = fromPersistValues reorder
    where clean (PersistMap x) = filter (\(k, _) -> T.head k /= '_') x
          reorder = match (map (T.pack . columnName) $ (entityColumns e)) (clean doc) []
              where match [] [] values = values
                    match (c:cs) fields values = let (found, unused) = matchOne fields []
                                                  in match cs unused (values ++ [snd found])
                        where matchOne (f:fs) tried = if c == fst f
                                                         then (f, tried ++ fs)
                                                         else matchOne fs (f:tried)
                              matchOne fs tried = error $ "reorder error: field doesn't match"
                                                          ++ (show c) ++ (show fs) ++ (show tried)
                    match cs fs values = error $ "reorder error: fields don't match"
                                                 ++ (show cs) ++ (show fs) ++ (show values)

modify :: (JSON a, MonadIO m) => (t -> a -> IO a) -> Key backend entity -> t -> CouchReader m ()
modify f k v = do
    let doc = keyToDoc k
    (conn, db) <- CouchReader ask
    _ <- run conn $ DB.getAndUpdateDoc db doc (f v)
    return ()

defaultView :: EntityDef -> [String] -> String -> String
defaultView t names extra = viewBody . viewConstraints (map columnName $ entityColumns t)
                            $ if null extra then viewEmit names [] else extra

viewBody :: String -> String
viewBody x = "(function (doc) {" ++ x ++ "})"

viewConstraints :: [String] -> String -> String
viewConstraints [] y = y
viewConstraints xs y = "if (" ++ (intercalate " && " $ map ("doc."++) xs) ++ ") {" ++ y ++ "}"

viewEmit :: [String] -> [String] -> String
viewEmit [] _ = viewEmit ["_id"] []
viewEmit xs ys = "emit(" ++ array xs ++ ", " ++ object ys ++ ");"
    where array [] = "doc._id"
          array [x] = "doc." ++ x
          array xs = "[" ++ (intercalate ", " $ map ("doc."++) xs) ++ "]"
          object [] = "doc._id"
          object [x] = "doc." ++ x
          object xs = "{" ++ (intercalate ", " $ map (\x -> "\"" ++ x ++ "\": " ++ "doc." ++ x) xs) ++ "}"

viewName :: [String] -> String
viewName [] = "default"
viewName xs = intercalate "_" xs

uniqueViewName :: [String] -> String -> String
uniqueViewName names text = viewName names ++ "_" ++ (showDigest . sha1 $ BL.pack text)

viewFilters :: (PersistEntity val) => [Filter val] -> String -> String
viewFilters [] x = x
viewFilters fs x = "if (" ++ (intercalate " && " $ map fKind fs) ++ ") {" ++ x ++ "}"
    where fKind (Filter field v NotIn) = "!(" ++ fKind (Filter field v In) ++ ")"
          fKind (Filter field v op) = "doc." ++ (columnName $ persistColumnDef field) ++
                                      fOp op ++ either (encode . showJSON . toPersistValue)
                                                       (encode . JSArray . map (showJSON . toPersistValue)) v
          fKind (FilterOr fs) = "(" ++ (intercalate " || " $ map fKind fs) ++ ")"
          fKind (FilterAnd fs) = "(" ++ (intercalate " && " $ map fKind fs) ++ ")"
          fOp Eq = " == "
          fOp Ne = " != "
          fOp Gt = " > "
          fOp Lt = " < "
          fOp Ge = " >= "
          fOp Le = " <= "
          fOp In = " in "

filtersToNames :: (PersistEntity val) => [Filter val] -> [String]
filtersToNames = nub . concatMap f
    where f (Filter field _ _) = [columnName $ persistColumnDef field]
          f (FilterOr fs) = concatMap f fs
          f (FilterAnd fs) = concatMap f fs

opts :: [SelectOpt a] -> [(String, JSValue)]
opts = nubBy (\(x, _) (y, _) -> x == "descending" && x == y) . map o
    -- The Asc and Desc options should be attribute dependent. Now, they just handle reversing of the output.
    where o (Asc _) = ("descending", JSBool False)
          o (Desc _) = ("descending", JSBool True)
          o (OffsetBy x) = ("skip", JSRational False $ fromIntegral x)
          o (LimitTo x) = ("limit", JSRational False $ fromIntegral x)

designName :: EntityDef -> DB.Doc
designName t = DB.doc . (\(x:xs) -> toLower x : xs) $ entityName t

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

runView :: (JSON a, MonadIO m) => DB.CouchConn -> DB.DB -> DB.Doc -> String -> [(String, JSValue)] -> [DB.CouchView] -> m [(DB.Doc, a)]
runView conn db design name dict views =
    let query = run conn $ DB.queryView db design (DB.doc name) dict
        -- The DB.newView function from the Database.CouchDB v 0.10 module is broken
        -- and fails with the HTTP 409 error when it is called more than once.
        -- Since there is no way to manipulate the _design area directly, we are using
        -- a modified version of the module.
        create = run conn $ DB.newView (show db) (show design) views
    in liftIO $ E.catch query (\(E.ErrorCall _) -> create >> query)

-- This is not a very effective solution, since it takes the whole input in once. It should be rewritten completely.
select :: (PersistEntity val, MonadIO m) => [Filter val] -> [(String, JSValue)]
    -> Step a' (CouchReader m) b -> [String] -> ((DB.Doc, PersistValue) -> a') -> Iteratee a' (CouchReader m) b
select f o (Continue k) vals process = do
    let names = filtersToNames f
        t = entityDef $ dummyFromFilts f
        design = designName t
        filters = viewFilters f $ viewEmit names vals
        name = uniqueViewName names filters
    (conn, db) <- lift $ CouchReader ask
    x <- runView conn db design name o [DB.ViewMap name $ defaultView t names filters]
    returnI $$ k . Chunks $ map process x

instance (MonadIO m, MonadBaseControl IO m) => PersistBackend CouchReader m where
    insert v = do
        (conn, db) <- CouchReader ask
        (doc, _) <- run conn $ DB.newDoc db (entityToJSON v)
        return $ docToKey doc

    replace = modify $ const . return . entityToJSON

    update k = modify (\u x -> return $ foldr field x u) k
        where e = entityDef $ dummyFromKey k
              field f@(Update _ value up) x = case up of
                                                   Assign -> execute x $ const v
                                                   Add -> execute x $ op (+) v
                                                   Subtract -> execute x $ op (-) v
                                                   Multiply -> execute x $ op (*) v
                                                   Divide -> execute x $ op (/) v
                  where name = T.pack $ updateFieldName f
                        v = toPersistValue value
                        execute (PersistMap x) g = PersistMap $ map (\(k, v) -> if k == name then (k, g v) else (k, v)) x
                        op o (PersistInt64 x) (PersistInt64 y) = PersistInt64 . truncate $ (fromIntegral y) `o` (fromIntegral x)
                        op o (PersistDouble x) (PersistDouble y) = PersistDouble $ y `o` x

    updateWhere f u = run_ $ selectKeys f $$ EL.mapM_ (flip update u)

    delete k = do
        let doc = keyToDoc k
        (conn, db) <- CouchReader ask
        _ <- run conn $ DB.forceDeleteDoc db doc
        return ()

    deleteBy u = do
        x <- getBy u
        when (isJust x)
             (delete . fst $ fromJust x)

    deleteWhere f = run_ $ selectKeys f $$ EL.mapM_ delete

    get k = do
        let doc = keyToDoc k
        (conn, db) <- CouchReader ask
        result <- run conn $ DB.getDoc db doc
        return $ maybe Nothing (\(_, _, v) -> either (\e -> error $ "Get error: " ++ e) Just $
                                              wrapFromPersistValues (entityDef $ dummyFromKey k) v) result

    getBy u = do
        let names = persistUniqueToFieldNames u
            values = uniqueToJSON $ persistUniqueToValues u
            t = entityDef $ dummyFromUnique u
            name = viewName names
            design = designName t
        (conn, db) <- CouchReader ask
        x <- runView conn db design name [("key", values)] [DB.ViewMap name $ defaultView t names ""]
        let justKey = fmap (\(k, _) -> docToKey k) $ maybeHead (x :: [(DB.Doc, PersistValue)])
        if isNothing justKey
           then return Nothing
           else do let key = fromJust justKey
                   y <- get key
                   return $ fmap (\v -> (key, v)) y

    selectEnum f o k = let t = entityDef $ dummyFromFilts f
                        in select f (opts o) k (map columnName $ entityColumns t)
                                  (\(x, y) -> (docToKey x, either (\e -> error $ "SelectEnum error: " ++ e)
                                                                  id $ wrapFromPersistValues t y))

    selectKeys f k = select f [] k [] (docToKey . fst)

    -- It is more effective to use a MapReduce view with the _count function, but the Database.CouchDB module
    -- expects the id attribute to be present in the result, which is e.g. {"rows":[{"key":null,"value":10}]}.
    -- For now, it is possible to write a custom function or to catch the exception and parse the count from it,
    -- but that is just plain ugly.
    count f = run_ $ selectKeys f $$ EL.fold ((flip . const) (+1)) 0

-- | Information required to connect to a CouchDB database.
data CouchConf = CouchConf
    { couchDatabase :: String
    , couchHost     :: String
    , couchPort     :: Int
    , couchPoolSize :: Int
    }

instance PersistConfig CouchConf where
    type PersistConfigBackend CouchConf = CouchReader
    type PersistConfigPool CouchConf = ConnectionPool
    withPool (CouchConf db host port poolsize) = withCouchDBPool db host port poolsize
    runPool _ = runCouchDBConn
    loadConfig e' = meither Left Right $ do
        e <- go $ fromMapping e'
        db <- go $ lookupScalar "database" e
        host <- go $ lookupScalar "host" e
        pool <- (go $ lookupScalar "poolsize" e) >>= safeRead "poolsize"
        port <- (go $ lookupScalar "port" e) >>= safeRead "port"

        return $ CouchConf { couchDatabase = T.unpack db
                           , couchHost = T.unpack host
                           , couchPort = port
                           , couchPoolSize = pool
                           }
      where
        go :: MEither ObjectExtractError a -> MEither String a
        go (MLeft e) = MLeft $ show e
        go (MRight a) = MRight a

safeRead :: String -> T.Text -> MEither String Int
safeRead name t = case reads s of
    (i, _):_ -> MRight i
    []       -> MLeft $ concat ["Invalid value for ", name, ": ", s]
  where
    s = T.unpack t
