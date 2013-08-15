{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
#ifndef NO_OVERLAP
{-# LANGUAGE OverlappingInstances #-}
#endif
module Database.Persist.Class.PersistField
    ( PersistField (..)
    , SomePersistField (..)
    , getPersistMap
    ) where

import Database.Persist.Types.Base
import Data.Time (Day(..), TimeOfDay, UTCTime)
#ifdef HIGH_PRECISION_DATE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
#endif
import Data.Time.LocalTime (ZonedTime)
import Data.ByteString.Char8 (ByteString, unpack)
import Control.Applicative
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Text (Text)
import Data.Fixed
import Data.Monoid ((<>))

import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as L

import Control.Monad ((<=<))

import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import qualified Data.Aeson as A

import qualified Data.Set as S
import qualified Data.Map as M

import qualified Data.Text.Encoding as TE

-- | A value which can be marshalled to and from a 'PersistValue'.
class PersistField a where
    toPersistValue :: a -> PersistValue
    fromPersistValue :: PersistValue -> Either T.Text a

#ifndef NO_OVERLAP
instance PersistField String where
    toPersistValue = PersistText . T.pack
    fromPersistValue (PersistText s) = Right $ T.unpack s
    fromPersistValue (PersistByteString bs) =
        Right $ T.unpack $ T.decodeUtf8With T.lenientDecode bs
    fromPersistValue (PersistInt64 i) = Right $ Prelude.show i
    fromPersistValue (PersistDouble d) = Right $ Prelude.show d
    fromPersistValue (PersistRational r) = Right $ Prelude.show r
    fromPersistValue (PersistDay d) = Right $ Prelude.show d
    fromPersistValue (PersistTimeOfDay d) = Right $ Prelude.show d
    fromPersistValue (PersistUTCTime d) = Right $ Prelude.show d
    fromPersistValue (PersistZonedTime (ZT z)) = Right $ Prelude.show z
    fromPersistValue PersistNull = Left $ T.pack "Unexpected null"
    fromPersistValue (PersistBool b) = Right $ Prelude.show b
    fromPersistValue (PersistList _) = Left $ T.pack "Cannot convert PersistList to String"
    fromPersistValue (PersistMap _) = Left $ T.pack "Cannot convert PersistMap to String"
    fromPersistValue (PersistDbSpecific _) = Left $ T.pack "Cannot convert PersistDbSpecific to String"
    fromPersistValue (PersistObjectId _) = Left $ T.pack "Cannot convert PersistObjectId to String"
#endif

instance PersistField ByteString where
    toPersistValue = PersistByteString
    fromPersistValue (PersistByteString bs) = Right bs
    fromPersistValue x = T.encodeUtf8 <$> fromPersistValue x

instance PersistField T.Text where
    toPersistValue = PersistText
    fromPersistValue = either (Left . T.pack) Right . fromPersistValueText

instance PersistField TL.Text where
    toPersistValue = toPersistValue . TL.toStrict
    fromPersistValue = fmap TL.fromStrict . fromPersistValue

instance PersistField Html where
    toPersistValue = PersistText . TL.toStrict . renderHtml
    fromPersistValue = fmap (preEscapedToMarkup :: T.Text -> Html) . fromPersistValue

instance PersistField Int where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ T.pack $ "Expected Integer, received: " ++ show x

instance PersistField Int8 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ T.pack $ "Expected Integer, received: " ++ show x

instance PersistField Int16 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ T.pack $ "Expected Integer, received: " ++ show x

instance PersistField Int32 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ T.pack $ "Expected Integer, received: " ++ show x

instance PersistField Int64 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ T.pack $ "Expected Integer, received: " ++ show x

instance PersistField Word where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ T.pack $ "Expected Word, received: " ++ show x

instance PersistField Word8 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ T.pack $ "Expected Word, received: " ++ show x

instance PersistField Word16 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ T.pack $ "Expected Word, received: " ++ show x

instance PersistField Word32 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ T.pack $ "Expected Word, received: " ++ show x

instance PersistField Word64 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ T.pack $ "Expected Word, received: " ++ show x

instance PersistField Double where
    toPersistValue = PersistDouble
    fromPersistValue (PersistDouble d) = Right d
    fromPersistValue (PersistRational r) = Right $ fromRational r
    fromPersistValue x = Left $ T.pack $ "Expected Double, received: " ++ show x

instance (HasResolution a) => PersistField (Fixed a) where
  toPersistValue = PersistRational . toRational
  fromPersistValue (PersistRational r) = Right $ fromRational r
  fromPersistValue (PersistText t) = case reads $ T.unpack t of --  NOTE: Sqlite can store rationals just as string
    [(a, "")] -> Right a
    _ -> Left $ "Can not read " <> t <> " as Fixed"
  fromPersistValue (PersistDouble d) = Right $ realToFrac d
  fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
  fromPersistValue x = Left $ "Expected Rational, received: " <> T.pack (show x)

instance PersistField Rational where
  toPersistValue = PersistRational
  fromPersistValue (PersistRational r) = Right r
  fromPersistValue (PersistDouble d) = Right $ toRational d
  fromPersistValue (PersistText t) = case reads $ T.unpack t of --  NOTE: Sqlite can store rationals just as string
    [(a, "")] -> Right $ toRational (a :: Pico)
    _ -> Left $ "Can not read " <> t <> " as Rational (Pico in fact)"
  fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
  fromPersistValue x = Left $ "Expected Rational, received: " <> T.pack (show x)

instance PersistField Bool where
    toPersistValue = PersistBool
    fromPersistValue (PersistBool b) = Right b
    fromPersistValue (PersistInt64 i) = Right $ i /= 0
    fromPersistValue x = Left $ T.pack $ "Expected Bool, received: " ++ show x

instance PersistField Day where
    toPersistValue = PersistDay
    fromPersistValue (PersistDay d) = Right d
    fromPersistValue (PersistInt64 i) = Right $ ModifiedJulianDay $ toInteger i
    fromPersistValue x@(PersistText t) =
        case reads $ T.unpack t of
            (d, _):_ -> Right d
            _ -> Left $ T.pack $ "Expected Day, received " ++ show x
    fromPersistValue x@(PersistByteString s) =
        case reads $ unpack s of
            (d, _):_ -> Right d
            _ -> Left $ T.pack $ "Expected Day, received " ++ show x
    fromPersistValue x = Left $ T.pack $ "Expected Day, received: " ++ show x

instance PersistField TimeOfDay where
    toPersistValue = PersistTimeOfDay
    fromPersistValue (PersistTimeOfDay d) = Right d
    fromPersistValue x@(PersistText t) =
        case reads $ T.unpack t of
            (d, _):_ -> Right d
            _ -> Left $ T.pack $ "Expected TimeOfDay, received " ++ show x
    fromPersistValue x@(PersistByteString s) =
        case reads $ unpack s of
            (d, _):_ -> Right d
            _ -> Left $ T.pack $ "Expected TimeOfDay, received " ++ show x
    fromPersistValue x = Left $ T.pack $ "Expected TimeOfDay, received: " ++ show x

instance PersistField UTCTime where
    toPersistValue = PersistUTCTime
    fromPersistValue (PersistUTCTime d) = Right d
#ifdef HIGH_PRECISION_DATE
    fromPersistValue (PersistInt64 i)   = Right $ posixSecondsToUTCTime $ (/ (1000 * 1000 * 1000)) $ fromIntegral $ i
#endif
    fromPersistValue x@(PersistText t)  =
        case reads $ T.unpack t of
            (d, _):_ -> Right d
            _ -> Left $ T.pack $ "Expected UTCTime, received " ++ show x
    fromPersistValue x@(PersistByteString s) =
        case reads $ unpack s of
            (d, _):_ -> Right d
            _ -> Left $ T.pack $ "Expected UTCTime, received " ++ show x

    fromPersistValue x = Left $ T.pack $ "Expected UTCTime, received: " ++ show x

instance PersistField ZonedTime where
    toPersistValue = PersistZonedTime . ZT
    fromPersistValue (PersistZonedTime (ZT z)) = Right z
    fromPersistValue x@(PersistText t) =
        case reads $ T.unpack t of
            (z, _):_ -> Right z
            _ -> Left $ T.pack $ "Expected ZonedTime, received " ++ show x
    fromPersistValue x@(PersistByteString s) =
        case reads $ unpack s of
            (z, _):_ -> Right z
            _ -> Left $ T.pack $ "Expected ZonedTime, received " ++ show x
    fromPersistValue x = Left $ T.pack $ "Expected ZonedTime, received: " ++ show x

instance PersistField a => PersistField (Maybe a) where
    toPersistValue Nothing = PersistNull
    toPersistValue (Just a) = toPersistValue a
    fromPersistValue PersistNull = Right Nothing
    fromPersistValue x = fmap Just $ fromPersistValue x

instance PersistField a => PersistField [a] where
    toPersistValue = PersistList . map toPersistValue
    fromPersistValue (PersistList l) = fromPersistList l
    fromPersistValue (PersistText t) = fromPersistValue (PersistByteString $ TE.encodeUtf8 t)
    fromPersistValue (PersistByteString bs)
        | Just values <- A.decode' (L.fromChunks [bs]) = fromPersistList values
    -- avoid the need for a migration to fill in empty lists.
    -- also useful when Persistent is not the only one filling in the data
    fromPersistValue (PersistNull) = Right []
    fromPersistValue x = Left $ T.pack $ "Expected PersistList, received: " ++ show x

instance (Ord a, PersistField a) => PersistField (S.Set a) where
    toPersistValue = PersistList . map toPersistValue . S.toList
    fromPersistValue (PersistList list) =
      either Left (Right . S.fromList) $ fromPersistList list
    fromPersistValue (PersistText t) = fromPersistValue (PersistByteString $ TE.encodeUtf8 t)
    fromPersistValue (PersistByteString bs)
        | Just values <- A.decode' (L.fromChunks [bs]) =
            either Left (Right . S.fromList) $ fromPersistList values
    fromPersistValue x = Left $ T.pack $ "Expected PersistSet, received: " ++ show x

instance (PersistField a, PersistField b) => PersistField (a,b) where
    toPersistValue (x,y) = PersistList [toPersistValue x, toPersistValue y]
    fromPersistValue (PersistList (vx:vy:[])) =
      case (fromPersistValue vx, fromPersistValue vy) of
        (Right x, Right y) -> Right (x, y)
        (Left e, _) -> Left e
        (_, Left e) -> Left e
    fromPersistValue x = Left $ T.pack $ "Expected 2 item PersistList, received: " ++ show x

instance PersistField v => PersistField (M.Map T.Text v) where
    toPersistValue = PersistMap . map (\(k,v) -> (k, toPersistValue v)) . M.toList
    fromPersistValue = fromPersistMap <=< getPersistMap

instance PersistField PersistValue where
    toPersistValue = id
    fromPersistValue = Right

deriving instance PersistField (KeyBackend backend entity)

fromPersistList :: PersistField a => [PersistValue] -> Either T.Text [a]
fromPersistList = mapM fromPersistValue

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

getPersistMap :: PersistValue -> Either T.Text [(T.Text, PersistValue)]
getPersistMap (PersistMap kvs) = Right kvs
getPersistMap (PersistText t)  = getPersistMap (PersistByteString $ TE.encodeUtf8 t)
getPersistMap (PersistByteString bs)
    | Just pairs <- A.decode' (L.fromChunks [bs]) = Right pairs
getPersistMap x = Left $ T.pack $ "Expected PersistMap, received: " ++ show x

data SomePersistField = forall a. PersistField a => SomePersistField a
instance PersistField SomePersistField where
    toPersistValue (SomePersistField a) = toPersistValue a
    fromPersistValue x = fmap SomePersistField (fromPersistValue x :: Either Text Text)

instance PersistField Checkmark where
    toPersistValue Active   = PersistBool True
    toPersistValue Inactive = PersistNull
    fromPersistValue PersistNull         = Right Inactive
    fromPersistValue (PersistBool True)  = Right Active
    fromPersistValue (PersistBool False) =
      Left $ T.pack "PersistField Checkmark: found unexpected FALSE value"
    fromPersistValue other =
      Left $ T.pack $ "PersistField Checkmark: unknown value " ++ show other
