{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
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
    , PersistOrder (..)
    , SomePersistField (..)
    , selectList
    ) where

import Language.Haskell.TH.Syntax
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.ByteString.Char8 (ByteString, unpack)
import qualified Data.ByteString.UTF8 as BSU
import Control.Applicative
import Data.Typeable (Typeable)
import Data.Int (Int64)
import Text.Blaze
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Monad (liftM)

-- | A raw value which can be stored in any backend and can be marshalled to
-- and from a 'PersistField'.
data PersistValue = PersistString String
                  | PersistByteString ByteString
                  | PersistInt64 Int64
                  | PersistDouble Double
                  | PersistBool Bool
                  | PersistDay Day
                  | PersistTimeOfDay TimeOfDay
                  | PersistUTCTime UTCTime
                  | PersistNull
    deriving (Show, Read, Eq, Typeable)

-- | A SQL data type. Naming attempts to reflect the underlying Haskell
-- datatypes, eg SqlString instead of SqlVarchar. Different SQL databases may
-- have different translations for these types.
data SqlType = SqlString
             | SqlInteger
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
    toPersistValue = PersistString
    fromPersistValue (PersistString s) = Right s
    fromPersistValue (PersistByteString bs) = Right $ BSU.toString bs
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
    fromPersistValue x = BSU.fromString <$> fromPersistValue x
    sqlType _ = SqlBlob

instance PersistField T.Text where
    toPersistValue = PersistString . T.unpack
    fromPersistValue = fmap T.pack . fromPersistValue
    sqlType _ = SqlString

instance PersistField (Html ()) where
    toPersistValue = PersistByteString . S.concat . L.toChunks . renderHtml
    fromPersistValue = fmap unsafeByteString . fromPersistValue
    sqlType _ = SqlString

instance PersistField Int where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Integer, received: " ++ show x
    sqlType _ = SqlInteger

instance PersistField Int64 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Integer, received: " ++ show x
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
    fromPersistValue x@(PersistString s) =
        case reads s of
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
    fromPersistValue x@(PersistString s) =
        case reads s of
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
    fromPersistValue x@(PersistString s) =
        case reads s of
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
    toPersistKey :: Int64 -> Key val
    fromPersistKey :: Key val -> Int64
    showPersistKey :: Key val -> String

    persistFilterToFieldName :: Filter val -> String
    persistFilterToFilter :: Filter val -> PersistFilter
    persistFilterIsNull :: Filter val -> Bool
    persistFilterToValue :: Filter val -> PersistValue

    persistOrderToFieldName :: Order val -> String
    persistOrderToOrder :: Order val -> PersistOrder

    persistUpdateToFieldName :: Update val -> String
    persistUpdateToValue :: Update val -> PersistValue

    persistUniqueToFieldNames :: Unique val -> [String]
    persistUniqueToValues :: Unique val -> [PersistValue]

data SomePersistField = forall a. PersistField a => SomePersistField a
instance PersistField SomePersistField where
    toPersistValue (SomePersistField a) = toPersistValue a
    fromPersistValue x = fmap SomePersistField (fromPersistValue x :: Either String String)
    sqlType (SomePersistField a) = sqlType a

class PersistBackend m where
    -- | Prepare database for this entity, if necessary. In SQL, this creates
    -- values and indices if they don't exist. The first argument is not used,
    -- so you can used 'undefined'.
    initialize :: PersistEntity val => val -> m ()

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
    select :: PersistEntity val
           => [Filter val]
           -> [Order val]
           -> Int -- ^ limit
           -> Int -- ^ offset
           -> a -- ^ iteration seed
           -> (a -> (Key val, val) -> m (Either a a))
           -> m (Either a a)

    -- | The total number of records fulfilling the given criterion.
    count :: PersistEntity val => [Filter val] -> m Int

-- | Call 'select' but return the result as a list.
selectList :: (PersistEntity val, PersistBackend m, Monad m)
           => [Filter val]
           -> [Order val]
           -> Int -- ^ limit
           -> Int -- ^ offset
           -> m [(Key val, val)]
selectList a b c d =
    either ($ []) ($ []) `liftM` select a b c d id iter
  where
    iter front a = return $ Right $ front . (:) a

data EntityDef = EntityDef
    { entityName    :: String
    , entityAttribs :: [String]
    , entityColumns :: [(String, String, [String])] -- ^ name, type, attribs
    , entityUniques :: [(String, [String])] -- ^ name, columns
    , entityDerives :: [String]
    }
    deriving Show

instance Lift EntityDef where
    lift (EntityDef a b c d e) = do
        x <- [|EntityDef|]
        a' <- lift a
        b' <- lift b
        c' <- lift c
        d' <- lift d
        e' <- lift e
        return $ x `AppE` a' `AppE` b' `AppE` c' `AppE` d' `AppE` e'

data PersistFilter = Eq | Ne | Gt | Lt | Ge | Le
    deriving (Read, Show)

instance Lift PersistFilter where
    lift Eq = [|Eq|]
    lift Ne = [|Ne|]
    lift Gt = [|Gt|]
    lift Lt = [|Lt|]
    lift Ge = [|Ge|]
    lift Le = [|Le|]

data PersistOrder = Asc | Desc
    deriving (Read, Show)

instance Lift PersistOrder where
    lift Asc = [|Asc|]
    lift Desc = [|Desc|]
