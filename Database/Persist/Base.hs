{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | This defines the API for performing database actions. There are two levels
-- to this API: dealing with fields, and dealing with entities. In SQL, a field
-- corresponds to a column, and should be a single, non-composite value. An
-- entity corresponds to a SQL table.  In other words: An entity is a
-- collection of fields.
module Database.Persist.Base
    ( PersistValue (..)
    , SqlType (..)
    , PersistField (..)
    , PersistEntity (..)
    , EntityDef (..)
    , PersistBackend (..)
    , PersistFilter (..)
    , PersistUpdate (..)
    , PersistOrder (..)
    , SomePersistField (..)
    , selectList
    , insertBy
    , checkUnique
    , DeleteCascade (..)
    , deleteCascadeWhere
    , PersistException (..)
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
import Data.Enumerator
import qualified Control.Exception as E
import Data.Bits (bitSize)
import Control.Monad (liftM)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

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
    deriving (Show, Read, Eq, Typeable, Ord)

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
    sqlType _ = SqlString

instance PersistField ByteString where
    toPersistValue = PersistByteString
    fromPersistValue (PersistByteString bs) = Right bs
    fromPersistValue x = T.encodeUtf8 <$> fromPersistValue x
    sqlType _ = SqlBlob

instance PersistField T.Text where
    toPersistValue = PersistText
    fromPersistValue (PersistByteString bs) =
        Right $ T.decodeUtf8With T.lenientDecode bs
    fromPersistValue v = fmap T.pack $ fromPersistValue v
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
    -- | The unique identifier associated with this entity. In general, backends also define a type synonym for this, such that \"type MyEntityId = Key MyEntity\".
    data Key    val
    -- | Fields which can be updated using the 'update' and 'updateWhere'
    -- functions.
    data Update val
    -- | Filters which are available for 'select', 'updateWhere' and
    -- 'deleteWhere'. Each filter constructor specifies the field being
    -- filtered on, the type of comparison applied (equals, not equals, etc)
    -- and the argument for the comparison.
    data Filter val
    -- | How you can sort the results of a 'select'.
    data Order  val
    -- | Unique keys in existence on this entity.
    data Unique val

    entityDef :: val -> EntityDef
    toPersistFields :: val -> [SomePersistField]
    fromPersistValues :: [PersistValue] -> Either String val
    halfDefined :: val
    toPersistKey :: PersistValue -> Key val
    fromPersistKey :: Key val -> PersistValue
    showPersistKey :: Key val -> String

    persistFilterToFieldName :: Filter val -> String
    persistFilterToFilter :: Filter val -> PersistFilter
    persistFilterToValue :: Filter val -> Either PersistValue [PersistValue]

    persistOrderToFieldName :: Order val -> String
    persistOrderToOrder :: Order val -> PersistOrder

    persistUpdateToFieldName :: Update val -> String
    persistUpdateToUpdate :: Update val -> PersistUpdate
    persistUpdateToValue :: Update val -> PersistValue

    persistUniqueToFieldNames :: Unique val -> [String]
    persistUniqueToValues :: Unique val -> [PersistValue]
    persistUniqueKeys :: val -> [Unique val]

data SomePersistField = forall a. PersistField a => SomePersistField a
instance PersistField SomePersistField where
    toPersistValue (SomePersistField a) = toPersistValue a
    fromPersistValue x = fmap SomePersistField (fromPersistValue x :: Either String String)
    sqlType (SomePersistField a) = sqlType a

class Monad m => PersistBackend m where
    -- | Create a new record in the database, returning the newly created
    -- identifier.
    insert :: PersistEntity val => val -> m (Key val)

    -- | Replace the record in the database with the given key. Result is
    -- undefined if such a record does not exist.
    replace :: PersistEntity val => Key val -> val -> m ()

    -- | Update individual fields on a specific record.
    update :: PersistEntity val => Key val -> [Update val] -> m ()

    -- | Update individual fields on any record matching the given criterion.
    updateWhere :: PersistEntity val => [Filter val] -> [Update val] -> m ()

    -- | Delete a specific record by identifier. Does nothing if record does
    -- not exist.
    delete :: PersistEntity val => Key val -> m ()

    -- | Delete a specific record by unique key. Does nothing if no record
    -- matches.
    deleteBy :: PersistEntity val => Unique val -> m ()

    -- | Delete all records matching the given criterion.
    deleteWhere :: PersistEntity val => [Filter val] -> m ()

    -- | Get a record by identifier, if available.
    get :: PersistEntity val => Key val -> m (Maybe val)

    -- | Get a record by unique key, if available. Returns also the identifier.
    getBy :: PersistEntity val => Unique val -> m (Maybe (Key val, val))

    -- | Get all records matching the given criterion in the specified order.
    -- Returns also the identifiers.
    selectEnum
           :: PersistEntity val
           => [Filter val]
           -> [Order val]
           -> Int -- ^ limit
           -> Int -- ^ offset
           -> Enumerator (Key val, val) m a

    -- | Get the 'Key's of all records matching the given criterion.
    selectKeys :: PersistEntity val
               => [Filter val]
               -> Enumerator (Key val) m a

    -- | The total number of records fulfilling the given criterion.
    count :: PersistEntity val => [Filter val] -> m Int

-- | Insert a value, checking for conflicts with any unique constraints.  If a
-- duplicate exists in the database, it is returned as 'Left'. Otherwise, the
-- new 'Key' is returned as 'Right'.
insertBy :: (PersistEntity v, PersistBackend m)
          => v -> m (Either (Key v, v) (Key v))
insertBy val =
    go $ persistUniqueKeys val
  where
    go [] = Right `liftM` insert val
    go (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> go xs
            Just z -> return $ Left z

-- | Check whether there are any conflicts for unique keys with this entity and
-- existing entities in the database.
--
-- Returns 'True' if the entity would be unique, and could thus safely be
-- 'insert'ed; returns 'False' on a conflict.
checkUnique :: (PersistEntity val, PersistBackend m) => val -> m Bool
checkUnique val =
    go $ persistUniqueKeys val
  where
    go [] = return True
    go (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> go xs
            Just _ -> return False

-- | Call 'select' but return the result as a list.
selectList :: (PersistEntity val, PersistBackend m, Monad m)
           => [Filter val]
           -> [Order val]
           -> Int -- ^ limit
           -> Int -- ^ offset
           -> m [(Key val, val)]
selectList a b c d = do
    res <- run $ selectEnum a b c d ==<< consume
    case res of
        Left e -> error $ show e
        Right x -> return x

data EntityDef = EntityDef
    { entityName    :: String
    , entityAttribs :: [String]
    , entityColumns :: [(String, String, [String])] -- ^ name, type, attribs
    , entityUniques :: [(String, [String])] -- ^ name, columns
    , entityDerives :: [String]
    }
    deriving Show

data PersistFilter = Eq | Ne | Gt | Lt | Ge | Le | In | NotIn
    deriving (Read, Show)

data PersistOrder = Asc | Desc
    deriving (Read, Show)

class PersistEntity a => DeleteCascade a where
    deleteCascade :: PersistBackend m => Key a -> m ()

deleteCascadeWhere :: (DeleteCascade a, PersistBackend m)
                   => [Filter a] -> m ()
deleteCascadeWhere filts = do
    res <- run $ selectKeys filts $ Continue iter
    case res of
        Left e -> error $ show e
        Right () -> return ()
  where
    iter EOF = Iteratee $ return $ Yield () EOF
    iter (Chunks keys) = Iteratee $ do
        mapM_ deleteCascade keys
        return $ Continue iter

data PersistException = PersistMarshalException String
    deriving (Show, Typeable)
instance E.Exception PersistException

data PersistUpdate = Update | Add | Subtract | Multiply | Divide
    deriving (Read, Show)

instance PersistField PersistValue where
    toPersistValue = id
    fromPersistValue = Right
    sqlType _ = SqlInteger -- since PersistValue should only be used like this for keys, which in SQL are Int64
