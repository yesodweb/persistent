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
{-# LANGUAGE PatternGuards #-}
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

    , ZT (..) -- ZonedTime wrapper

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

      -- * Helpers
    , getPersistMap
    , listToJSON
    , mapToJSON
    , Key'
    , Unique'

      -- * Config
    , PersistConfig (..)
    ) where

import qualified Prelude
import Prelude hiding ((++), show)
import Data.Monoid (mappend)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC, zonedTimeToLocalTime, zonedTimeZone)
import Data.ByteString.Char8 (ByteString, unpack)
import Control.Applicative
import Data.Typeable (Typeable)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as L

import qualified Control.Exception as E
import Control.Monad.Trans.Error (Error (..))
import Database.Persist.EntityDef

import Data.Bits (bitSize)
import Control.Monad (liftM, (<=<))
import Control.Arrow (second)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Web.PathPieces (PathPiece (..))
import qualified Data.Text.Read

import Data.Aeson (Value)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Number as AN
import qualified Data.Vector as V

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as B64

import Data.Aeson (toJSON)
import Data.Aeson.Encode (fromValue)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid (Monoid)

import Data.Conduit (Pipe)
import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT   )
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.Cont     ( ContT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )
import Control.Monad.Trans.Resource ( ResourceT, MonadResource )

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

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

-- | Avoid orphan instances.
newtype ZT = ZT ZonedTime deriving (Show, Read, Typeable)

instance Eq ZT where
    ZT a /= ZT b = zonedTimeToLocalTime a /= zonedTimeToLocalTime b || zonedTimeZone a /= zonedTimeZone b
instance Ord ZT where
    ZT a `compare` ZT b = zonedTimeToUTC a `compare` zonedTimeToUTC b

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
                  | PersistZonedTime ZT
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

instance A.ToJSON PersistValue where
    toJSON (PersistText t) = A.String $ T.cons 's' t
    toJSON (PersistByteString b) = A.String $ T.cons 'b' $ TE.decodeUtf8 $ B64.encode b
    toJSON (PersistInt64 i) = A.Number $ fromIntegral i
    toJSON (PersistDouble d) = A.Number $ AN.D d
    toJSON (PersistBool b) = A.Bool b
    toJSON (PersistTimeOfDay t) = A.String $ T.cons 't' $ show t
    toJSON (PersistUTCTime u) = A.String $ T.cons 'u' $ show u
    toJSON (PersistZonedTime z) = A.String $ T.cons 'z' $ show z
    toJSON (PersistDay d) = A.String $ T.cons 'd' $ show d
    toJSON PersistNull = A.Null
    toJSON (PersistList l) = A.Array $ V.fromList $ map A.toJSON l
    toJSON (PersistMap m) = A.object $ map (second A.toJSON) m
    toJSON (PersistObjectId o) = A.String $ T.cons 'o' $ TE.decodeUtf8 $ B64.encode o

instance A.FromJSON PersistValue where
    parseJSON (A.String t0) =
        case T.uncons t0 of
            Nothing -> fail "Null string"
            Just ('s', t) -> return $ PersistText t
            Just ('b', t) -> either (fail "Invalid base64") (return . PersistByteString)
                           $ B64.decode $ TE.encodeUtf8 t
            Just ('t', t) -> fmap PersistTimeOfDay $ readMay t
            Just ('u', t) -> fmap PersistUTCTime $ readMay t
            Just ('z', t) -> fmap PersistZonedTime $ readMay t
            Just ('d', t) -> fmap PersistDay $ readMay t
            Just ('o', t) -> either (fail "Invalid base64") (return . PersistObjectId)
                           $ B64.decode $ TE.encodeUtf8 t
            Just (c, _) -> fail $ "Unknown prefix: " `mappend` [c]
      where
        readMay :: (Read a, Monad m) => T.Text -> m a
        readMay t =
            case reads $ T.unpack t of
                (x, _):_ -> return x
                [] -> fail "Could not read"
    parseJSON (A.Number (AN.I i)) = return $ PersistInt64 $ fromInteger i
    parseJSON (A.Number (AN.D d)) = return $ PersistDouble d
    parseJSON (A.Bool b) = return $ PersistBool b
    parseJSON A.Null = return $ PersistNull
    parseJSON (A.Array a) = fmap PersistList (mapM A.parseJSON $ V.toList a)
    parseJSON (A.Object o) =
        fmap PersistMap $ mapM go $ HM.toList o
      where
        go (k, v) = fmap ((,) k) $ A.parseJSON v

-- | A SQL data type. Naming attempts to reflect the underlying Haskell
-- datatypes, eg SqlString instead of SqlVarchar. Different SQL databases may
-- have different translations for these types.
data SqlType = SqlString
             | SqlInt32
             | SqlInt64
             | SqlReal
             | SqlBool
             | SqlDay
             | SqlTime
             | SqlDayTime
             | SqlDayTimeZoned
             | SqlBlob
             | SqlOther T.Text -- ^ a backend-specific name
    deriving (Show, Read, Eq, Typeable, Ord)

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
    fromPersistValue (PersistZonedTime (ZT z)) = Right $ Prelude.show z
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
    fromPersistValue (PersistZonedTime (ZT z)) = Right $ show z
    fromPersistValue PersistNull = Left "Unexpected null"
    fromPersistValue (PersistBool b) = Right $ show b
    fromPersistValue (PersistList _) = Left "Cannot convert PersistList to Text"
    fromPersistValue (PersistMap _) = Left "Cannot convert PersistMap to Text"
    fromPersistValue (PersistObjectId _) = Left "Cannot convert PersistObjectId to Text"
    sqlType _ = SqlString

instance PersistField Html where
    toPersistValue = PersistText . TL.toStrict . renderHtml
    fromPersistValue = fmap (preEscapedToMarkup :: T.Text -> Html) . fromPersistValue
    sqlType _ = SqlString

instance PersistField Int where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Integer, received: " ++ show x
    sqlType x = case bitSize x of
                    32 -> SqlInt32
                    _ -> SqlInt64

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
    sqlType _ = SqlInt64

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
    sqlType _ = SqlInt64

instance PersistField Word64 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "Expected Wordeger, received: " ++ show x
    sqlType _ = SqlInt64

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

instance PersistField ZonedTime where
    toPersistValue = PersistZonedTime . ZT
    fromPersistValue (PersistZonedTime (ZT z)) = Right z
    fromPersistValue x@(PersistText t) =
        case reads $ T.unpack t of
            (z, _):_ -> Right z
            _ -> Left $ "Expected ZonedTime, received " ++ show x
    fromPersistValue x@(PersistByteString s) =
        case reads $ unpack s of
            (z, _):_ -> Right z
            _ -> Left $ "Expected ZonedTime, received " ++ show x
    fromPersistValue x = Left $ "Expected ZonedTime, received: " ++ show x
    sqlType _ = SqlDayTimeZoned

instance PersistField a => PersistField (Maybe a) where
    toPersistValue Nothing = PersistNull
    toPersistValue (Just a) = toPersistValue a
    fromPersistValue PersistNull = Right Nothing
    fromPersistValue x = fmap Just $ fromPersistValue x
    sqlType _ = sqlType (error "this is the problem" :: a)
    isNullable _ = True

-- | Helper wrapper, equivalent to @Unique val (PersistEntityBackend val)@.
--
-- Since 1.1.0
type Unique' val = Unique val (PersistEntityBackend val)

-- | Helper wrapper, equivalent to @Key (PersistEntityBackend val) val@.
--
-- Since 1.1.0
type Key' val = Key (PersistEntityBackend val) val

-- | A single database entity. For example, if writing a blog application, a
-- blog entry would be an entry, containing fields such as title and content.
class PersistEntity val where
    -- | Parameters: val and datatype of the field
    data EntityField val :: * -> *
    persistFieldDef :: EntityField val typ -> FieldDef

    type PersistEntityBackend val

    -- | Unique keys in existence on this entity.
    data Unique val :: * -> *

    entityDef :: val -> EntityDef
    toPersistFields :: val -> [SomePersistField]
    fromPersistValues :: [PersistValue] -> Either T.Text val
    halfDefined :: val

    persistUniqueToFieldNames :: Unique val backend -> [(HaskellName, DBName)]
    persistUniqueToValues :: Unique val backend -> [PersistValue]
    persistUniqueKeys :: val -> [Unique val backend]

    persistIdField :: EntityField val (Key (PersistEntityBackend val) val)

instance PersistField a => PersistField [a] where
    toPersistValue = PersistList . map toPersistValue
    fromPersistValue (PersistList l) = fromPersistList l
    fromPersistValue (PersistText t) = fromPersistValue (PersistByteString $ TE.encodeUtf8 t)
    fromPersistValue (PersistByteString bs)
        | Just values <- A.decode' (L.fromChunks [bs]) = fromPersistList values
    fromPersistValue x = Left $ "Expected PersistList, received: " ++ show x
    sqlType _ = SqlString

instance (Ord a, PersistField a) => PersistField (S.Set a) where
    toPersistValue = PersistList . map toPersistValue . S.toList
    fromPersistValue (PersistList list) =
      either Left (Right . S.fromList) $ fromPersistList list
    fromPersistValue (PersistText t) = fromPersistValue (PersistByteString $ TE.encodeUtf8 t)
    fromPersistValue (PersistByteString bs)
        | Just values <- A.decode' (L.fromChunks [bs]) =
            either Left (Right . S.fromList) $ fromPersistList values
    fromPersistValue x = Left $ "Expected PersistSet, received: " ++ show x
    sqlType _ = SqlString

fromPersistList :: PersistField a => [PersistValue] -> Either T.Text [a]
fromPersistList = mapM fromPersistValue

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
    fromPersistValue = fromPersistMap <=< getPersistMap
    sqlType _ = SqlString

getPersistMap :: PersistValue -> Either T.Text [(T.Text, PersistValue)]
getPersistMap (PersistMap kvs) = Right kvs
getPersistMap (PersistText t)  = getPersistMap (PersistByteString $ TE.encodeUtf8 t)
getPersistMap (PersistByteString bs)
    | Just pairs <- A.decode' (L.fromChunks [bs]) = Right pairs
getPersistMap x = Left $ "Expected PersistMap, received: " ++ show x

fromPersistMap :: PersistField v
               => [(T.Text, PersistValue)]
               -> Either T.Text (M.Map T.Text v)
fromPersistMap kvs =
      case (
        foldl (\eithAssocs (k,v) ->
              case (eithAssocs, fromPersistValue v) of
                (Left e, _) -> Left e
                (_, Left e)   -> Left e
                (Right assocs, Right v') -> Right ((k,v'):assocs)
              ) (Right []) kvs
      ) of
        Right vs -> Right $ M.fromList vs
        Left e -> Left e

data SomePersistField = forall a. PersistField a => SomePersistField a
instance PersistField SomePersistField where
    toPersistValue (SomePersistField a) = toPersistValue a
    fromPersistValue x = fmap SomePersistField (fromPersistValue x :: Either T.Text T.Text)
    sqlType (SomePersistField a) = sqlType a

newtype Key backend entity = Key { unKey :: PersistValue }
    deriving (Show, Read, Eq, Ord, PersistField)

instance A.ToJSON (Key backend entity) where
    toJSON (Key val) = A.toJSON val

instance A.FromJSON (Key backend entity) where
    parseJSON = fmap Key . A.parseJSON

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

class MonadResource m => PersistStore m where
    type PersistMonadBackend m

    -- | Create a new record in the database, returning an automatically created
    -- key (in SQL an auto-increment id).
    insert :: PersistEntity val => val -> m (Key (PersistMonadBackend m) val)

    -- | Create a new record in the database using the given key.
    insertKey :: PersistEntity val => Key (PersistMonadBackend m) val -> val -> m ()

    -- | Put the record in the database with the given key.
    -- Unlike 'replace', if a record with the given key does not
    -- exist then a new record will be inserted.
    repsert :: PersistEntity val => Key (PersistMonadBackend m) val -> val -> m ()

    -- | Replace the record in the database with the given
    -- key. Note that the result is undefined if such record does
    -- not exist, so you must use 'insertKey' or 'repsert' in
    -- these cases.
    replace :: PersistEntity val => Key (PersistMonadBackend m) val -> val -> m ()

    -- | Delete a specific record by identifier. Does nothing if record does
    -- not exist.
    delete :: PersistEntity val => Key (PersistMonadBackend m) val -> m ()

    -- | Get a record by identifier, if available.
    get :: PersistEntity val => Key (PersistMonadBackend m) val -> m (Maybe val)

#define DEF(T) { type PersistMonadBackend (T m) = PersistMonadBackend m; insert = lift . insert; insertKey k = lift . insertKey k; repsert k = lift . repsert k; replace k = lift . replace k; delete = lift . delete; get = lift . get }
#define GO(T) instance (PersistStore m) => PersistStore (T m) where DEF(T)
#define GOX(X, T) instance (X, PersistStore m) => PersistStore (T m) where DEF(T)

GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(ContT r)
GO(StateT s)
GO(ResourceT)
GO(Pipe l i o u)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)

#undef DEF
#undef GO
#undef GOX

class PersistStore m => PersistUnique m where
    -- | Get a record by unique key, if available. Returns also the identifier.
    getBy :: (PersistEntityBackend val ~ PersistMonadBackend m, PersistEntity val) => Unique' val -> m (Maybe (Entity val))

    -- | Delete a specific record by unique key. Does nothing if no record
    -- matches.
    deleteBy :: (PersistEntityBackend val ~ PersistMonadBackend m, PersistEntity val) => Unique' val -> m ()

    -- | Like 'insert', but returns 'Nothing' when the record
    -- couldn't be inserted because of a uniqueness constraint.
    insertUnique :: (PersistEntityBackend val ~ PersistMonadBackend m, PersistEntity val) => val -> m (Maybe (Key' val))
    insertUnique datum = do
        isUnique <- checkUnique datum
        if isUnique then Just `liftM` insert datum else return Nothing



-- | Insert a value, checking for conflicts with any unique constraints.  If a
-- duplicate exists in the database, it is returned as 'Left'. Otherwise, the
-- new 'Key' is returned as 'Right'.
insertBy :: (PersistEntity v, PersistStore m, PersistUnique m, PersistMonadBackend m ~ PersistEntityBackend v)
          => v -> m (Either (Entity v) (Key' v))
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
getByValue :: (PersistEntity v, PersistUnique m, PersistEntityBackend v ~ PersistMonadBackend m)
           => v -> m (Maybe (Entity v))
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
  (PersistStore m
  , PersistEntity ent1
  , PersistEntity ent2
  , PersistMonadBackend m ~ PersistEntityBackend ent2
  ) => (ent1 -> Maybe (Key' ent2)) -> ent1 -> m (Maybe ent2)
belongsTo foreignKeyField model = case foreignKeyField model of
    Nothing -> return Nothing
    Just f -> get f

-- | same as belongsTo, but uses @getJust@ and therefore is similarly unsafe
belongsToJust ::
  (PersistStore m
  , PersistEntity ent1
  , PersistEntity ent2
  , PersistMonadBackend m ~ PersistEntityBackend ent2)
  => (ent1 -> Key' ent2) -> ent1 -> m ent2
belongsToJust getForeignKey model = getJust $ getForeignKey model

-- | Same as get, but for a non-null (not Maybe) foreign key
--   Unsafe unless your database is enforcing that the foreign key is valid
getJust :: (PersistStore m, PersistEntity val, Show (Key' val), PersistMonadBackend m ~ PersistEntityBackend val) => Key' val -> m val
getJust key = get key >>= maybe
  (liftIO $ E.throwIO $ PersistForeignConstraintUnmet $ show key)
  return


-- | Check whether there are any conflicts for unique keys with this entity and
-- existing entities in the database.
--
-- Returns 'True' if the entity would be unique, and could thus safely be
-- 'insert'ed; returns 'False' on a conflict.
checkUnique :: (PersistEntityBackend val ~ PersistMonadBackend m, PersistEntity val, PersistUnique m) => val -> m Bool
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

class PersistEntity a => DeleteCascade a where
    deleteCascade :: (PersistStore m, PersistEntityBackend a ~ PersistMonadBackend m) => Key' a -> m ()

instance PersistField PersistValue where
    toPersistValue = id
    fromPersistValue = Right
    sqlType _ = SqlInt64 -- since PersistValue should only be used like this for keys, which in SQL are Int64

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
    runPool :: (MonadBaseControl IO m, MonadIO m)
            => c
            -> PersistConfigBackend c m a
            -> PersistConfigPool c
            -> m a

infixr 5 ++
(++) :: T.Text -> T.Text -> T.Text
(++) = mappend

show :: Show a => a -> T.Text
show = T.pack . Prelude.show

listToJSON :: [PersistValue] -> T.Text
listToJSON = toStrict . toLazyText . fromValue . toJSON

mapToJSON :: [(T.Text, PersistValue)] -> T.Text
mapToJSON = toStrict . toLazyText . fromValue . toJSON
