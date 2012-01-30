{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- This is to test our assumption that OverlappingInstances is just for String
#ifndef NO_OVERLAP
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | API for database actions. The API deals with fields and entities.
-- In SQL, a field corresponds to a column, and should be a single non-composite value.
-- An entity corresponds to a SQL table, so an entity is a collection of fields.
module Database.Persist.Store
    ( PersistValue (..)
    , SqlType (..)
    , PersistField (..)
    , PersistEntity (..)
    , PersistStore (..)
    , PersistUnique (..)
    , PersistFilter (..)
    , SomePersistField (..)

    , insertBy
    , getByValue
    , getJust
    , belongsTo
    , belongsToJust

    , checkUnique
    , DeleteCascade (..)
    , PersistException (..)
    , Key (..)
    , Entity (..)

      -- * Config
    , PersistConfig (..)
    ) where

import qualified Prelude
import Prelude hiding ((++), show)
import Data.Monoid (mappend)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.ByteString.Char8 (ByteString, unpack)
import Control.Applicative
import Data.Typeable (Typeable)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Text.Blaze (Html, unsafeByteString)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Control.Monad.IO.Class as Trans

import qualified Control.Exception as E
import Control.Monad.Trans.Error (Error (..))
import Database.Persist.EntityDef

import Data.Bits (bitSize)
import Control.Monad (liftM)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Web.PathPieces (PathPiece (..))
import qualified Data.Text.Read
import qualified Data.Conduit as C

import Data.Aeson (Value)
import Data.Aeson.Types (Parser)

#ifdef WITH_MONGODB
import qualified Data.Set as S
import qualified Data.Map as M
#endif

data PersistException
  = PersistError T.Text -- ^ Generic Exception
  | PersistMarshalError T.Text
  | PersistInvalidField T.Text
  | PersistForeignConstraintUnmet T.Text
  | PersistMongoDBError T.Text
  | PersistMongoDBUnsupported T.Text
    deriving (Show, Typeable)

instance E.Exception PersistException
instance Error PersistException where
    strMsg = PersistError . T.pack

-- | A raw value which can be stored in any backend and can be marshalled to
-- and from a 'PersistField'.
data PersistValue = PersistText T.Text
                  | PersistByteString ByteString
                  | PersistInt64 Int64
                  | PersistDouble Double
                  | PersistBool Bool
                  | PersistDay Day
                  | PersistTimeOfDay TimeOfDay
                  | PersistUTCTime UTCTime
                  | PersistNull
                  | PersistList [PersistValue]
                  | PersistMap [(T.Text, PersistValue)]
                  | PersistObjectId ByteString -- ^ intended especially for MongoDB backend
    deriving (Show, Read, Eq, Typeable, Ord)

instance PathPiece PersistValue where
    fromPathPiece t =
        case Data.Text.Read.signed Data.Text.Read.decimal t of
            Right (i, t')
                | T.null t' -> Just $ PersistInt64 i
            _ -> Just $ PersistText t
    toPathPiece x =
        case fromPersistValue x of
            Left e -> error $ T.unpack e
            Right y -> y

-- | A SQL data type. Naming attempts to reflect the underlying Haskell
-- datatypes, eg SqlString instead of SqlVarchar. Different SQL databases may
-- have different translations for these types.
data SqlType = SqlString
             | SqlInt32
             | SqlInteger -- ^ FIXME 8-byte integer; should be renamed SqlInt64
             | SqlReal
             | SqlBool
             | SqlDay
             | SqlTime
             | SqlDayTime
             | SqlBlob
    deriving (Show, Read, Eq, Typeable)

-- | A value which can be marshalled to and from a 'PersistValue'.
class PersistField a where
    toPersistValue :: a -> PersistValue
    fromPersistValue :: PersistValue -> Either T.Text a
    sqlType :: a -> SqlType
    isNullable :: a -> Bool
    isNullable _ = False

#ifndef NO_OVERLAP
instance PersistField String where
    toPersistValue = PersistText . T.pack
    fromPersistValue (PersistText s) = Right $ T.unpack s
    fromPersistValue (PersistByteString bs) =
        Right $ T.unpack $ T.decodeUtf8With T.lenientDecode bs
    fromPersistValue (PersistInt64 i) = Right $ Prelude.show i
    fromPersistValue (PersistDouble d) = Right $ Prelude.show d
    fromPersistValue (PersistDay d) = Right $ Prelude.show d
    fromPersistValue (PersistTimeOfDay d) = Right $ Prelude.show d
    fromPersistValue (PersistUTCTime d) = Right $ Prelude.show d
    fromPersistValue PersistNull = Left "Unexpected null"
    fromPersistValue (PersistBool b) = Right $ Prelude.show b
    fromPersistValue (PersistList _) = Left "Cannot convert PersistList to String"
    fromPersistValue (PersistMap _) = Left "Cannot convert PersistMap to String"
    fromPersistValue (PersistObjectId _) = Left "Cannot convert PersistObjectId to String"
    sqlType _ = SqlString
#endif

instance PersistField ByteString where
    toPersistValue = PersistByteString
    fromPersistValue (PersistByteString bs) = Right bs
    fromPersistValue x = T.encodeUtf8 <$> fromPersistValue x
    sqlType _ = SqlBlob

instance PersistField T.Text where
    toPersistValue = PersistText
    fromPersistValue (PersistText s) = Right s
    fromPersistValue (PersistByteString bs) =
        Right $ T.decodeUtf8With T.lenientDecode bs
    fromPersistValue (PersistInt64 i) = Right $ show i
    fromPersistValue (PersistDouble d) = Right $ show d
    fromPersistValue (PersistDay d) = Right $ show d
    fromPersistValue (PersistTimeOfDay d) = Right $ show d
    fromPersistValue (PersistUTCTime d) = Right $ show d
    fromPersistValue PersistNull = Left "Unexpected null"
    fromPersistValue (PersistBool b) = Right $ show b
    fromPersistValue (PersistList _) = Left "Cannot convert PersistList to Text"
    fromPersistValue (PersistMap _) = Left "Cannot convert PersistMap to Text"
    fromPersistValue (PersistObjectId _) = Left "Cannot convert PersistObjectId to Text"
    sqlType _ = SqlString

instance PersistField Html where
    toPersistValue = PersistByteString . S.concat . L.toChunks . renderHtml
    fromPersistValue = fmap unsafeByteString . fromPersistValue
    sqlType _ = SqlString

instance PersistField Int where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Integer, received: " ++ show x
    sqlType x = case bitSize x of
                    32 -> SqlInt32
                    _ -> SqlInteger

instance PersistField Int8 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Integer, received: " ++ show x
    sqlType _ = SqlInt32

instance PersistField Int16 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Integer, received: " ++ show x
    sqlType _ = SqlInt32

instance PersistField Int32 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Integer, received: " ++ show x
    sqlType _ = SqlInt32

instance PersistField Int64 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Integer, received: " ++ show x
    sqlType _ = SqlInteger

instance PersistField Word8 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Wordeger, received: " ++ show x
    sqlType _ = SqlInt32

instance PersistField Word16 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Wordeger, received: " ++ show x
    sqlType _ = SqlInt32

instance PersistField Word32 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Wordeger, received: " ++ show x
    sqlType _ = SqlInteger

instance PersistField Word64 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Wordeger, received: " ++ show x
    sqlType _ = SqlInteger

instance PersistField Double where
    toPersistValue = PersistDouble
    fromPersistValue (PersistDouble d) = Right d
    fromPersistValue x = Left $ "Expected Double, received: " ++ show x
    sqlType _ = SqlReal

instance PersistField Bool where
    toPersistValue = PersistBool
    fromPersistValue (PersistBool b) = Right b
    fromPersistValue (PersistInt64 i) = Right $ i /= 0
    fromPersistValue x = Left $ "Expected Bool, received: " ++ show x
    sqlType _ = SqlBool

instance PersistField Day where
    toPersistValue = PersistDay
    fromPersistValue (PersistDay d) = Right d
    fromPersistValue x@(PersistText t) =
        case reads $ T.unpack t of
            (d, _):_ -> Right d
            _ -> Left $ "Expected Day, received " ++ show x
    fromPersistValue x@(PersistByteString s) =
        case reads $ unpack s of
            (d, _):_ -> Right d
            _ -> Left $ "Expected Day, received " ++ show x
    fromPersistValue x = Left $ "Expected Day, received: " ++ show x
    sqlType _ = SqlDay

instance PersistField TimeOfDay where
    toPersistValue = PersistTimeOfDay
    fromPersistValue (PersistTimeOfDay d) = Right d
    fromPersistValue x@(PersistText t) =
        case reads $ T.unpack t of
            (d, _):_ -> Right d
            _ -> Left $ "Expected TimeOfDay, received " ++ show x
    fromPersistValue x@(PersistByteString s) =
        case reads $ unpack s of
            (d, _):_ -> Right d
            _ -> Left $ "Expected TimeOfDay, received " ++ show x
    fromPersistValue x = Left $ "Expected TimeOfDay, received: " ++ show x
    sqlType _ = SqlTime

instance PersistField UTCTime where
    toPersistValue = PersistUTCTime
    fromPersistValue (PersistUTCTime d) = Right d
    fromPersistValue x@(PersistText t) =
        case reads $ T.unpack t of
            (d, _):_ -> Right d
            _ -> Left $ "Expected UTCTime, received " ++ show x
    fromPersistValue x@(PersistByteString s) =
        case reads $ unpack s of
            (d, _):_ -> Right d
            _ -> Left $ "Expected UTCTime, received " ++ show x
    fromPersistValue x = Left $ "Expected UTCTime, received: " ++ show x
    sqlType _ = SqlDayTime

instance PersistField a => PersistField (Maybe a) where
    toPersistValue Nothing = PersistNull
    toPersistValue (Just a) = toPersistValue a
    fromPersistValue PersistNull = Right Nothing
    fromPersistValue x = fmap Just $ fromPersistValue x
    sqlType _ = sqlType (error "this is the problem" :: a)
    isNullable _ = True

-- | A single database entity. For example, if writing a blog application, a
-- blog entry would be an entry, containing fields such as title and content.
class PersistEntity val where
    -- | Parameters: val and datatype of the field
    data EntityField val :: * -> *
    persistFieldDef :: EntityField val typ -> FieldDef

    type PersistEntityBackend val :: ((* -> *) -> * -> *)

    -- | Unique keys in existence on this entity.
    data Unique val :: ((* -> *) -> * -> *) -> *

    entityDef :: val -> EntityDef
    toPersistFields :: val -> [SomePersistField]
    fromPersistValues :: [PersistValue] -> Either T.Text val
    halfDefined :: val

    persistUniqueToFieldNames :: Unique val backend -> [(HaskellName, DBName)]
    persistUniqueToValues :: Unique val backend -> [PersistValue]
    persistUniqueKeys :: val -> [Unique val backend]

    persistIdField :: EntityField val (Key (PersistEntityBackend val) val)

#ifdef WITH_MONGODB
instance PersistField a => PersistField [a] where
    toPersistValue = PersistList . map toPersistValue
    fromPersistValue (PersistList l) = fromPersistList l
    fromPersistValue x = Left $ "Expected PersistList, received: " ++ show x
    sqlType _ = SqlString

instance (Ord a, PersistField a) => PersistField (S.Set a) where
    toPersistValue = PersistList . map toPersistValue . S.toList
    fromPersistValue (PersistList list) =
      either Left (Right . S.fromList) $ fromPersistList list
    fromPersistValue x = Left $ "Expected PersistList, received: " ++ show x
    sqlType _ = SqlString

fromPersistList :: PersistField a => [PersistValue] -> Either T.Text [a]
fromPersistList list =
        foldl (\eithList v ->
              case (eithList, fromPersistValue v) of
                (Left e, _)         -> Left e
                (_, Left e)         -> Left e
                (Right xs, Right x) -> Right (x:xs)
              ) (Right []) list

instance (PersistField a, PersistField b) => PersistField (a,b) where
    toPersistValue (x,y) = PersistList [toPersistValue x, toPersistValue y]
    fromPersistValue (PersistList (vx:vy:[])) =
      case (fromPersistValue vx, fromPersistValue vy) of
        (Right x, Right y) -> Right (x, y)
        (Left e, _) -> Left e
        (_, Left e) -> Left e
    fromPersistValue x = Left $ "Expected 2 item PersistList, received: " ++ show x
    sqlType _ = SqlString

instance PersistField v => PersistField (M.Map T.Text v) where
    toPersistValue = PersistMap . map (\(k,v) -> (k, toPersistValue v)) . M.toList
    fromPersistValue (PersistMap kvs) = case (
        foldl (\eithAssocs (k,v) ->
              case (eithAssocs, fromPersistValue v) of
                (Left e, _) -> Left e
                (_, Left e)   -> Left e
                (Right assocs, Right v') -> Right ((k,v'):assocs)
              ) (Right []) kvs
      ) of
        Right vs -> Right $ M.fromList vs
        Left e -> Left e

    fromPersistValue x = Left $ "Expected PersistMap, received: " ++ show x
    sqlType _ = SqlString
#endif


data SomePersistField = forall a. PersistField a => SomePersistField a
instance PersistField SomePersistField where
    toPersistValue (SomePersistField a) = toPersistValue a
    fromPersistValue x = fmap SomePersistField (fromPersistValue x :: Either T.Text T.Text)
    sqlType (SomePersistField a) = sqlType a

newtype Key (backend :: (* -> *) -> * -> *) entity = Key { unKey :: PersistValue }
    deriving (Show, Read, Eq, Ord, PersistField)

-- | Datatype that represents an entity, with both its key and
-- its Haskell representation.
--
-- When using the an SQL-based backend (such as SQLite or
-- PostgreSQL), an 'Entity' may take any number of columns
-- depending on how many fields it has. In order to reconstruct
-- your entity on the Haskell side, @persistent@ needs all of
-- your entity columns and in the right order.  Note that you
-- don't need to worry about this when using @persistent@\'s API
-- since everything is handled correctly behind the scenes.
--
-- However, if you want to issue a raw SQL command that returns
-- an 'Entity', then you have to be careful with the column
-- order.  While you could use @SELECT Entity.* WHERE ...@ and
-- that would work most of the time, there are times when the
-- order of the columns on your database is different from the
-- order that @persistent@ expects (for example, if you add a new
-- field in the middle of you entity definition and then use the
-- migration code -- @persistent@ will expect the column to be in
-- the middle, but your DBMS will put it as the last column).
-- So, instead of using a query like the one above, you may use
-- 'Database.Persist.GenericSql.rawSql' (from the
-- "Database.Persist.GenericSql" module) with its /entity
-- selection placeholder/ (a double question mark @??@).  Using
-- @rawSql@ the query above must be written as @SELECT ??  WHERE
-- ..@.  Then @rawSql@ will replace @??@ with the list of all
-- columns that we need from your entity in the right order.  If
-- your query returns two entities (i.e. @(Entity backend a,
-- Entity backend b)@), then you must you use @SELECT ??, ??
-- WHERE ...@, and so on.
data Entity entity =
    Entity { entityKey :: Key (PersistEntityBackend entity) entity
           , entityVal :: entity }
    deriving (Eq, Ord, Show, Read)

class (C.ResourceIO m, C.ResourceIO (b m)) => PersistStore b m where

    -- | Create a new record in the database, returning an automatically created
    -- key (in SQL an auto-increment id).
    insert :: PersistEntity val => val -> b m (Key b val)

    -- | Create a new record in the database using the given key.
    insertKey :: PersistEntity val => Key b val -> val -> b m ()

    -- | Put the record in the database with the given key.
    -- Unlike 'replace', if a record with the given key does not
    -- exist then a new record will be inserted.
    repsert :: PersistEntity val => Key b val -> val -> b m ()

    -- | Replace the record in the database with the given
    -- key. Note that the result is undefined if such record does
    -- not exist, so you must use 'insertKey' or 'repsert' in
    -- these cases.
    replace :: PersistEntity val => Key b val -> val -> b m ()

    -- | Delete a specific record by identifier. Does nothing if record does
    -- not exist.
    delete :: PersistEntity val => Key b val -> b m ()

    -- | Get a record by identifier, if available.
    get :: PersistEntity val => Key b val -> b m (Maybe val)

class PersistStore b m => PersistUnique b m where
    -- | Get a record by unique key, if available. Returns also the identifier.
    getBy :: (PersistEntityBackend val ~ b, PersistEntity val) => Unique val b -> b m (Maybe (Entity val))

    -- | Delete a specific record by unique key. Does nothing if no record
    -- matches.
    deleteBy :: PersistEntity val => Unique val b -> b m ()

    -- | Like 'insert', but returns 'Nothing' when the record
    -- couldn't be inserted because of a uniqueness constraint.
    insertUnique :: (b ~ PersistEntityBackend val, PersistEntity val) => val -> b m (Maybe (Key b val))
    insertUnique datum = do
        isUnique <- checkUnique datum
        if isUnique then Just <$> insert datum else return Nothing



-- | Insert a value, checking for conflicts with any unique constraints.  If a
-- duplicate exists in the database, it is returned as 'Left'. Otherwise, the
-- new 'Key' is returned as 'Right'.
insertBy :: (PersistEntity v, PersistStore b m, PersistUnique b m, b ~ PersistEntityBackend v)
          => v -> b m (Either (Entity v) (Key b v))
insertBy val =
    go $ persistUniqueKeys val
  where
    go [] = Right `liftM` insert val
    go (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> go xs
            Just z -> return $ Left z

-- | A modification of 'getBy', which takes the 'PersistEntity' itself instead
-- of a 'Unique' value. Returns a value matching /one/ of the unique keys. This
-- function makes the most sense on entities with a single 'Unique'
-- constructor.
getByValue :: (PersistEntity v, PersistUnique b m, PersistEntityBackend v ~ b)
           => v -> b m (Maybe (Entity v))
getByValue val =
    go $ persistUniqueKeys val
  where
    go [] = return Nothing
    go (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> go xs
            Just z -> return $ Just z

-- | curry this to make a convenience function that loads an associated model
--   > foreign = belongsTo foeignId
belongsTo ::
  (PersistStore b m
  , PersistEntity ent1
  , PersistEntity ent2) => (ent1 -> Maybe (Key b ent2)) -> ent1 -> b m (Maybe ent2)
belongsTo foreignKeyField model = case foreignKeyField model of
    Nothing -> return Nothing
    Just f -> get f

-- | same as belongsTo, but uses @getJust@ and therefore is similarly unsafe
belongsToJust ::
  (PersistStore b m
  , PersistEntity ent1
  , PersistEntity ent2) => (ent1 -> Key b ent2) -> ent1 -> b m ent2
belongsToJust getForeignKey model = getJust $ getForeignKey model

-- | Same as get, but for a non-null (not Maybe) foreign key
--   Unsafe unless your database is enforcing that the foreign key is valid
getJust :: (PersistStore b m, PersistEntity val, Show (Key b val)) => Key b val -> b m val
getJust key = get key >>= maybe
  (Trans.liftIO $ E.throwIO $ PersistForeignConstraintUnmet $ show key)
  return


-- | Check whether there are any conflicts for unique keys with this entity and
-- existing entities in the database.
--
-- Returns 'True' if the entity would be unique, and could thus safely be
-- 'insert'ed; returns 'False' on a conflict.
checkUnique :: (PersistEntityBackend val ~ b, PersistEntity val, PersistUnique b m) => val -> b m Bool
checkUnique val =
    go $ persistUniqueKeys val
  where
    go [] = return True
    go (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> go xs
            Just _ -> return False

data PersistFilter = Eq | Ne | Gt | Lt | Ge | Le | In | NotIn
                   | BackendSpecificFilter T.Text
    deriving (Read, Show)

class PersistEntity a => DeleteCascade a b m where
    deleteCascade :: Key b a -> b m ()

instance PersistField PersistValue where
    toPersistValue = id
    fromPersistValue = Right
    sqlType _ = SqlInteger -- since PersistValue should only be used like this for keys, which in SQL are Int64

-- | Represents a value containing all the configuration options for a specific
-- backend. This abstraction makes it easier to write code that can easily swap
-- backends.
class PersistConfig c where
    type PersistConfigBackend c :: (* -> *) -> * -> *
    type PersistConfigPool c

    -- | Load the config settings from a 'Value', most likely taken from a YAML
    -- config file.
    loadConfig :: Value -> Parser c

    -- | Modify the config settings based on environment variables.
    applyEnv :: c -> IO c
    applyEnv = return

    -- | Create a new connection pool based on the given config settings.
    createPoolConfig :: c -> IO (PersistConfigPool c)

    -- | Run a database action by taking a connection from the pool.
    runPool :: C.ResourceIO m => c -> PersistConfigBackend c m a
            -> PersistConfigPool c
            -> m a

infixr 5 ++
(++) :: T.Text -> T.Text -> T.Text
(++) = mappend

show :: Show a => a -> T.Text
show = T.pack . Prelude.show
