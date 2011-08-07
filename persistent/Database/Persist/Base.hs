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
    , PersistBackend (..)
    , PersistFilter (..)
    , PersistUpdate (..)
    , SelectOpt (..)
    , SomePersistField (..)
    , selectList
    , insertBy
    , getByValue
    , checkUnique
    , DeleteCascade (..)
    , deleteCascadeWhere
    , PersistException (..)
    , Update (..)
    , updateFieldName
    , Filter (..)
    , Key (..)
      -- * Definition
    , EntityDef (..)
    , ColumnName
    , ColumnType
    , ColumnDef (..)
    , UniqueDef (..)
    , fst3, snd3, third3
    , limitOffsetOrder
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
import Data.Enumerator hiding (consume, map)
import Data.Enumerator.List (consume)
import qualified Data.Enumerator.List as EL
import qualified Control.Exception as E
import Data.Bits (bitSize)
import Control.Monad (liftM)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Web.PathPieces (SinglePiece (..))
import qualified Data.Text.Read
import Data.Maybe (fromMaybe, isJust)

fst3 :: forall t t1 t2. (t, t1, t2) -> t
fst3   (x, _, _) = x
snd3 :: forall t t1 t2. (t, t1, t2) -> t1
snd3   (_, x, _) = x
third3 :: forall t t1 t2. (t, t1, t2) -> t2
third3 (_, _, x) = x

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

data Update v = forall typ. PersistField typ => Update
    { updateField :: Field v typ
    , updateValue :: typ
    , updateUpdate :: PersistUpdate -- FIXME Replace with expr down the road
    }

updateFieldName :: PersistEntity v => Update v -> String
updateFieldName (Update f _ _) = columnName $ persistColumnDef f

data SelectOpt v = forall typ. Asc (Field v typ)
                 | forall typ. Desc (Field v typ)
                 | OffsetBy Int
                 | LimitTo Int

-- | Filters which are available for 'select', 'updateWhere' and
-- 'deleteWhere'. Each filter constructor specifies the field being
-- filtered on, the type of comparison applied (equals, not equals, etc)
-- and the argument for the comparison.
data Filter v = forall typ. PersistField typ => Filter
    { filterField :: Field v typ
    , filterValue :: Either typ [typ] -- FIXME
    , filterFilter :: PersistFilter -- FIXME
    }
    | FilterAnd [Filter v] -- ^ convenient for internal use, not needed for the API
    | FilterOr [Filter v]

-- | A single database entity. For example, if writing a blog application, a
-- blog entry would be an entry, containing fields such as title and content.
class PersistEntity val where
    -- | Parameters: val and datatype of the field
    data Field val :: * -> *
    persistColumnDef :: Field val typ -> ColumnDef

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

class (Monad (b m), Monad m) => PersistBackend b m where

    -- | Create a new record in the database, returning the newly created
    -- identifier.
    insert :: PersistEntity val => val -> b m (Key b val)

    -- | Replace the record in the database with the given key. Result is
    -- undefined if such a record does not exist.
    replace :: PersistEntity val => Key b val -> val -> b m ()

    -- | Update individual fields on a specific record.
    update :: PersistEntity val => Key b val -> [Update val] -> b m ()

    -- | Update individual fields on any record matching the given criterion.
    updateWhere :: PersistEntity val => [Filter val] -> [Update val] -> b m ()

    -- | Delete a specific record by identifier. Does nothing if record does
    -- not exist.
    delete :: PersistEntity val => Key b val -> b m ()

    -- | Delete a specific record by unique key. Does nothing if no record
    -- matches.
    deleteBy :: PersistEntity val => Unique val b -> b m ()

    -- | Delete all records matching the given criterion.
    deleteWhere :: PersistEntity val => [Filter val] -> b m ()

    -- | Get a record by identifier, if available.
    get :: PersistEntity val => Key b val -> b m (Maybe val)

    -- | Get a record by unique key, if available. Returns also the identifier.
    getBy :: PersistEntity val => Unique val b -> b m (Maybe (Key b val, val))

    -- | Get all records matching the given criterion in the specified order.
    -- Returns also the identifiers.
    selectEnum
           :: PersistEntity val
           => [Filter val]
           -> [SelectOpt val]
           -> Enumerator (Key b val, val) (b m) a

    -- | get just the first record for the criterion
    selectFirst :: PersistEntity val
                => [Filter val]
                -> [SelectOpt val]
                -> b m (Maybe (Key b val, val))
    selectFirst filts opts = run_ $ selectEnum filts ((LimitTo 1):opts) ==<< EL.head


    -- | Get the 'Key's of all records matching the given criterion.
    selectKeys :: PersistEntity val
               => [Filter val]
               -> Enumerator (Key b val) (b m) a

    -- | The total number of records fulfilling the given criterion.
    count :: PersistEntity val => [Filter val] -> b m Int

-- | Insert a value, checking for conflicts with any unique constraints.  If a
-- duplicate exists in the database, it is returned as 'Left'. Otherwise, the
-- new 'Key' is returned as 'Right'.
insertBy :: (PersistEntity v, PersistBackend b m)
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
getByValue :: (PersistEntity v, PersistBackend b m)
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

-- | Check whether there are any conflicts for unique keys with this entity and
-- existing entities in the database.
--
-- Returns 'True' if the entity would be unique, and could thus safely be
-- 'insert'ed; returns 'False' on a conflict.
checkUnique :: (PersistEntity val, PersistBackend b m) => val -> b m Bool
checkUnique val =
    go $ persistUniqueKeys val
  where
    go [] = return True
    go (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> go xs
            Just _ -> return False

limitOffsetOrder :: PersistEntity val => [SelectOpt val] -> (Int, Int, [SelectOpt val])
limitOffsetOrder opts = let (l,o,ord) = go opts (Nothing, Nothing, []) in
    (fromMaybe 0 l, fromMaybe 0 o, ord)
  where
    go []              tup = tup
    go (LimitTo  x:xs) (_,o,ord) = let tup = (Just x, o, ord) in
      if isJust o then tup else go xs tup
    go (OffsetBy x:xs) (l,_,ord) = let tup = (l, Just x, ord) in
      if isJust l then tup else go xs tup
    go (x:xs)          (l, o, ord) = go xs (l,o,x:ord)

-- | Call 'select' but return the result as a list.
selectList :: (PersistEntity val, PersistBackend b m)
           => [Filter val]
           -> [SelectOpt val]
           -> b m [(Key b val, val)]
selectList a b = do
    res <- run $ selectEnum a b ==<< consume
    case res of
        Left e -> error $ show e
        Right x -> return x

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

class PersistEntity a => DeleteCascade a where
    deleteCascade :: PersistBackend b m => Key b a -> b m ()

deleteCascadeWhere :: (DeleteCascade a, PersistBackend b m)
                   => [Filter a] -> b m ()
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

data PersistUpdate = Assign | Add | Subtract | Multiply | Divide -- FIXME need something else here
    deriving (Read, Show, Enum, Bounded)

instance PersistField PersistValue where
    toPersistValue = id
    fromPersistValue = Right
    sqlType _ = SqlInteger -- since PersistValue should only be used like this for keys, which in SQL are Int64
