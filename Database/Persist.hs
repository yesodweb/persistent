{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Persist
    ( -- * Values
      PersistValue (..)
    , SqlType (..)
    , PersistField (..)
      -- * Type class
    , PersistEntity (..)
    ) where

import Language.Haskell.TH.Syntax
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.ByteString.Char8 (ByteString, unpack)
import qualified Data.ByteString.UTF8 as BSU
import Control.Applicative
import Data.Typeable (Typeable)
import Data.Int (Int64)
import Text.Hamlet
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

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

data SqlType = SqlString
             | SqlInteger
             | SqlReal
             | SqlBool
             | SqlDay
             | SqlTime
             | SqlDayTime
             | SqlBlob
    deriving (Show, Read, Eq, Typeable)

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

class PersistEntity val where
    type PersistMonad val :: * -> *

    data Key    val
    data Update val
    data Filter val
    data Order  val
    data Unique val

    -- initialization, value is ignored
    initialize  :: val                          -> (PersistMonad val) ()

    -- insert
    insert      :: val                          -> (PersistMonad val) (Key val)
    -- Removed for PostgreSQL insertR     :: val                          -> m (Key val)
    replace     :: Key val      -> val          -> (PersistMonad val) ()

    -- modify
    update      :: Key val      -> [Update val] -> (PersistMonad val) ()
    updateWhere :: [Filter val] -> [Update val] -> (PersistMonad val) ()

    -- delete
    delete      :: Key val                      -> (PersistMonad val) ()
    deleteBy    :: Unique val                   -> (PersistMonad val) ()
    deleteWhere :: [Filter val]                 -> (PersistMonad val) ()

    -- read single
    get         :: Key val                      -> (PersistMonad val) (Maybe val)
    getBy       :: Unique val                   -> (PersistMonad val) (Maybe (Key val, val))

    -- read multiple
    select      :: [Filter val] -> [Order val]  -> (PersistMonad val) [(Key val, val)]
