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

-- | API for database actions. The API deals with fields and entities.
-- In SQL, a field corresponds to a column, and should be a single non-composite value.
-- An entity corresponds to a SQL table, so an entity is a collection of fields.
module Database.Persist.Base
    ( PersistValue (..)
    , SqlType (..)
    , PersistField (..)
    , PersistEntity (..)
    , PersistStore (..)
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
      -- * Definition
    , EntityDef (..)
    , ColumnName
    , ColumnType
    , ColumnDef (..)
    , UniqueDef (..)
    , fst3, snd3, third3

      -- * Config
    , PersistConfig (..)
    ) where

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

import Data.Bits (bitSize)
import Control.Monad (liftM)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Web.PathPieces (SinglePiece (..))
import qualified Data.Text.Read

import Control.Monad.IO.Control (MonadControlIO)
import Data.Object (TextObject)

fst3 :: forall t t1 t2. (t, t1, t2) -> t
fst3   (x, _, _) = x
snd3 :: forall t t1 t2. (t, t1, t2) -> t1
snd3   (_, x, _) = x
third3 :: forall t t1 t2. (t, t1, t2) -> t2
third3 (_, _, x) = x

data PersistException
  = PersistError String -- ^ Generic Exception
  | PersistMarshalError String
  | PersistInvalidField String
  | PersistForeignConstraintUnmet String
  | PersistMongoDBError String
  | PersistMongoDBUnsupported String
    deriving (Show, Typeable)

instance E.Exception PersistException
instance Error PersistException where strMsg msg = PersistError msg



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

instance SinglePiece PersistValue where
    fromSinglePiece t =
        case Data.Text.Read.signed Data.Text.Read.decimal t of
            Right (i, t')
                | T.null t' -> Just $ PersistInt64 i
            _ -> Just $ PersistText t
    toSinglePiece x =
        case fromPersistValue x of
            Left e -> error e
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
    fromPersistValue :: PersistValue -> Either String a
    sqlType :: a -> SqlType
    isNullable :: a -> Bool
    isNullable _ = False

instance PersistField String where
    toPersistValue = PersistText . T.pack
    fromPersistValue (PersistText s) = Right $ T.unpack s
    fromPersistValue (PersistByteString bs) =
        Right $ T.unpack $ T.decodeUtf8With T.lenientDecode bs
    fromPersistValue (PersistInt64 i) = Right $ show i
    fromPersistValue (PersistDouble d) = Right $ show d
    fromPersistValue (PersistDay d) = Right $ show d
    fromPersistValue (PersistTimeOfDay d) = Right $ show d
    fromPersistValue (PersistUTCTime d) = Right $ show d
    fromPersistValue PersistNull = Left "Unexpected null"
    fromPersistValue (PersistBool b) = Right $ show b
    fromPersistValue (PersistList _) = Left "Cannot convert PersistList to String"
    fromPersistValue (PersistMap _) = Left "Cannot convert PersistMap to String"
    fromPersistValue (PersistObjectId _) = Left "Cannot convert PersistObjectId to String"
    sqlType _ = SqlString

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
    fromPersistValue (PersistInt64 i) = Right $ T.pack $ show i
    fromPersistValue (PersistDouble d) = Right $ T.pack $ show d
    fromPersistValue (PersistDay d) = Right $ T.pack $ show d
    fromPersistValue (PersistTimeOfDay d) = Right $ T.pack $ show d
    fromPersistValue (PersistUTCTime d) = Right $ T.pack $ show d
    fromPersistValue PersistNull = Left "Unexpected null"
    fromPersistValue (PersistBool b) = Right $ T.pack $ show b
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
    persistColumnDef :: EntityField val typ -> ColumnDef

    -- | Unique keys in existence on this entity.
    data Unique val :: ((* -> *) -> * -> *) -> *

    entityDef :: val -> EntityDef
    toPersistFields :: val -> [SomePersistField]
    fromPersistValues :: [PersistValue] -> Either String val
    halfDefined :: val

    persistUniqueToFieldNames :: Unique val backend -> [String]
    persistUniqueToValues :: Unique val backend -> [PersistValue]
    persistUniqueKeys :: val -> [Unique val backend]

data SomePersistField = forall a. PersistField a => SomePersistField a
instance PersistField SomePersistField where
    toPersistValue (SomePersistField a) = toPersistValue a
    fromPersistValue x = fmap SomePersistField (fromPersistValue x :: Either String String)
    sqlType (SomePersistField a) = sqlType a

newtype Key backend entity = Key { unKey :: PersistValue }
    deriving (Show, Read, Eq, Ord, PersistField)

class (Trans.MonadIO (b m), Trans.MonadIO m, Monad (b m), Monad m) => PersistStore b m where

    -- | Create a new record in the database, returning the newly created
    -- identifier.
    insert :: PersistEntity val => val -> b m (Key b val)

    -- | Replace the record in the database with the given key. Result is
    -- undefined if such a record does not exist.
    replace :: PersistEntity val => Key b val -> val -> b m ()

    -- | Delete a specific record by identifier. Does nothing if record does
    -- not exist.
    delete :: PersistEntity val => Key b val -> b m ()

    -- | Get a record by identifier, if available.
    get :: PersistEntity val => Key b val -> b m (Maybe val)

    -- | Get a record by unique key, if available. Returns also the identifier.
    getBy :: PersistEntity val => Unique val b -> b m (Maybe (Key b val, val))

    -- | Delete a specific record by unique key. Does nothing if no record
    -- matches.
    deleteBy :: PersistEntity val => Unique val b -> b m ()




-- | Insert a value, checking for conflicts with any unique constraints.  If a
-- duplicate exists in the database, it is returned as 'Left'. Otherwise, the
-- new 'Key' is returned as 'Right'.
insertBy :: (PersistEntity v, PersistStore b m)
          => v -> b m (Either (Key b v, v) (Key b v))
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
getByValue :: (PersistEntity v, PersistStore b m)
           => v -> b m (Maybe (Key b v, v))
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
  (Trans.liftIO . E.throwIO $ PersistForeignConstraintUnmet (show key))
  return


-- | Check whether there are any conflicts for unique keys with this entity and
-- existing entities in the database.
--
-- Returns 'True' if the entity would be unique, and could thus safely be
-- 'insert'ed; returns 'False' on a conflict.
checkUnique :: (PersistEntity val, PersistStore b m) => val -> b m Bool
checkUnique val =
    go $ persistUniqueKeys val
  where
    go [] = return True
    go (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> go xs
            Just _ -> return False

data EntityDef = EntityDef
    { entityName    :: String
    , entityAttribs :: [String]
    , entityColumns :: [ColumnDef]
    , entityUniques :: [UniqueDef]
    , entityDerives :: [String]
    }
    deriving Show

type ColumnName = String
type ColumnType = String

data ColumnDef = ColumnDef -- FIXME rename to FieldDef? Also, do we need a columnHaskellName and columnDBName?
    { columnName    :: ColumnName
    , columnType    :: ColumnType
    , columnAttribs :: [String]
    }
    deriving Show

data UniqueDef = UniqueDef
    { uniqueName    :: String
    , uniqueColumns :: [ColumnName]
    }
    deriving Show

data PersistFilter = Eq | Ne | Gt | Lt | Ge | Le | In | NotIn
                   | BackendSpecificFilter String
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
    loadConfig :: TextObject -> Either String c
    -- | I really don't want Applicative here, but it's necessary for Mongo.
    withPool :: (Applicative m, MonadControlIO m) => c -> (PersistConfigPool c -> m a) -> m a
    runPool :: MonadControlIO m => c -> PersistConfigBackend c m a
            -> PersistConfigPool c
            -> m a
