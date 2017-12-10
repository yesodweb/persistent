{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances #-}
#endif

module Database.Persist.Class.PersistField
    ( PersistField (..)
    , SomePersistField (..)
    , getPersistMap
    ) where

import Control.Arrow (second)
import Database.Persist.Types.Base
import Data.Time (Day(..), TimeOfDay, UTCTime,
#if MIN_VERSION_time(1,5,0)
    parseTimeM)
#else
    parseTime)
#endif
#ifdef HIGH_PRECISION_DATE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
#endif
import Data.ByteString.Char8 (ByteString, unpack, readInt)
import Control.Applicative as A
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Text (Text)
import Data.Text.Read (double)
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
import qualified Data.IntMap as IM

import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

#if MIN_VERSION_base(4,8,0)
import Numeric.Natural (Natural)
#endif

-- | A value which can be marshalled to and from a 'PersistValue'.
class PersistField a where
    toPersistValue :: a -> PersistValue
    fromPersistValue :: PersistValue -> Either T.Text a

#ifndef NO_OVERLAP
#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPPING #-} PersistField [Char] where
#else
instance PersistField [Char] where
#endif
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
    fromPersistValue PersistNull = Left $ T.pack "Unexpected null"
    fromPersistValue (PersistBool b) = Right $ Prelude.show b
    fromPersistValue (PersistList _) = Left $ T.pack "Cannot convert PersistList to String"
    fromPersistValue (PersistMap _) = Left $ T.pack "Cannot convert PersistMap to String"
    fromPersistValue (PersistDbSpecific _) = Left $ T.pack "Cannot convert PersistDbSpecific to String. See the documentation of PersistDbSpecific for an example of using a custom database type with Persistent."
    fromPersistValue (PersistObjectId _) = Left $ T.pack "Cannot convert PersistObjectId to String"
#endif

instance PersistField ByteString where
    toPersistValue = PersistByteString
    fromPersistValue (PersistByteString bs) = Right bs
    fromPersistValue x = T.encodeUtf8 A.<$> fromPersistValue x

instance PersistField T.Text where
    toPersistValue = PersistText
    fromPersistValue = fromPersistValueText

instance PersistField TL.Text where
    toPersistValue = toPersistValue . TL.toStrict
    fromPersistValue = fmap TL.fromStrict . fromPersistValue

instance PersistField Html where
    toPersistValue = PersistText . TL.toStrict . renderHtml
    fromPersistValue = fmap (preEscapedToMarkup :: T.Text -> Html) . fromPersistValue

instance PersistField Int where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i)  = Right $ fromIntegral i
    fromPersistValue (PersistDouble i) = Right (truncate i :: Int) -- oracle
    fromPersistValue x = Left $ T.pack $ "int Expected Integer, received: " ++ show x

instance PersistField Int8 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i)  = Right $ fromIntegral i
    fromPersistValue (PersistDouble i) = Right (truncate i :: Int8) -- oracle
    fromPersistValue (PersistByteString bs) = case readInt bs of  -- oracle
                                               Just (i,"") -> Right $ fromIntegral i
                                               xs -> error $ "PersistField Int8 failed parsing PersistByteString xs["++show xs++"] i["++show bs++"]"
    fromPersistValue x = Left $ T.pack $ "int8 Expected Integer, received: " ++ show x

instance PersistField Int16 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i)  = Right $ fromIntegral i
    fromPersistValue (PersistDouble i) = Right (truncate i :: Int16) -- oracle
    fromPersistValue (PersistByteString bs) = case readInt bs of  -- oracle
                                               Just (i,"") -> Right $ fromIntegral i
                                               xs -> error $ "PersistField Int16 failed parsing PersistByteString xs["++show xs++"] i["++show bs++"]"
    fromPersistValue x = Left $ T.pack $ "int16 Expected Integer, received: " ++ show x

instance PersistField Int32 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i)  = Right $ fromIntegral i
    fromPersistValue (PersistDouble i) = Right (truncate i :: Int32) -- oracle
    fromPersistValue (PersistByteString bs) = case readInt bs of  -- oracle
                                               Just (i,"") -> Right $ fromIntegral i
                                               xs -> error $ "PersistField Int32 failed parsing PersistByteString xs["++show xs++"] i["++show bs++"]"
    fromPersistValue x = Left $ T.pack $ "int32 Expected Integer, received: " ++ show x

instance PersistField Int64 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i)  = Right $ fromIntegral i
    fromPersistValue (PersistDouble i) = Right (truncate i :: Int64) -- oracle
    fromPersistValue (PersistByteString bs) = case readInt bs of  -- oracle
                                               Just (i,"") -> Right $ fromIntegral i
                                               xs -> error $ "PersistField Int64 failed parsing PersistByteString xs["++show xs++"] i["++show bs++"]"
    fromPersistValue x = Left $ T.pack $ "int64 Expected Integer, received: " ++ show x

instance PersistField Data.Word.Word where
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
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ T.pack $ "Expected Double, received: " ++ show x

instance (HasResolution a) => PersistField (Fixed a) where
    toPersistValue = PersistRational . toRational
    fromPersistValue (PersistRational r) = Right $ fromRational r
    fromPersistValue (PersistText t) = case reads $ T.unpack t of --  NOTE: Sqlite can store rationals just as string
      [(a, "")] -> Right a
      _ -> Left $ "Can not read " <> t <> " as Fixed"
    fromPersistValue (PersistDouble d) = Right $ realToFrac d
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ "PersistField Fixed:Expected Rational, received: " <> T.pack (show x)

instance PersistField Rational where
    toPersistValue = PersistRational
    fromPersistValue (PersistRational r) = Right r
    fromPersistValue (PersistDouble d) = Right $ toRational d
    fromPersistValue (PersistText t) = case reads $ T.unpack t of --  NOTE: Sqlite can store rationals just as string
      [(a, "")] -> Right $ toRational (a :: Pico)
      _ -> Left $ "Can not read " <> t <> " as Rational (Pico in fact)"
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue (PersistByteString bs) = case double $ T.cons '0' $ T.decodeUtf8With T.lenientDecode bs of
                                                Right (ret,"") -> Right $ toRational ret
                                                Right (a,b) -> Left $ "Invalid bytestring[" <> T.pack (show bs) <> "]: expected a double but returned " <> T.pack (show (a,b))
                                                Left xs -> Left $ "Invalid bytestring[" <> T.pack (show bs) <> "]: expected a double but returned " <> T.pack (show xs)
    fromPersistValue x = Left $ "PersistField Rational:Expected Rational, received: " <> T.pack (show x)

instance PersistField Bool where
    toPersistValue = PersistBool
    fromPersistValue (PersistBool b) = Right b
    fromPersistValue (PersistInt64 i) = Right $ i /= 0
    fromPersistValue (PersistByteString i) = case readInt i of
                                               Just (0,"") -> Right False
                                               Just (1,"") -> Right True
                                               xs -> error $ "PersistField Bool failed parsing PersistByteString xs["++show xs++"] i["++show i++"]"
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
            _ ->
                case parse8601 $ T.unpack t of
                    Nothing -> Left $ T.pack $ "Expected UTCTime, received " ++ show x
                    Just x' -> Right x'
      where
#if MIN_VERSION_time(1,5,0)
        parse8601 = parseTimeM True defaultTimeLocale "%FT%T%Q"
#else
        parse8601 = parseTime defaultTimeLocale "%FT%T%Q"
#endif
    fromPersistValue x@(PersistByteString s) =
        case reads $ unpack s of
            (d, _):_ -> Right d
            _ -> Left $ T.pack $ "Expected UTCTime, received " ++ show x

    fromPersistValue x = Left $ T.pack $ "Expected UTCTime, received: " ++ show x

#if MIN_VERSION_base(4,8,0)
instance PersistField Natural where
  toPersistValue = (toPersistValue :: Int64 -> PersistValue) . fromIntegral
  fromPersistValue x = fromIntegral <$> (fromPersistValue x :: Either Text Int64)
#endif

instance PersistField a => PersistField (Maybe a) where
    toPersistValue Nothing = PersistNull
    toPersistValue (Just a) = toPersistValue a
    fromPersistValue PersistNull = Right Nothing
    fromPersistValue x = Just <$> fromPersistValue x

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPPABLE #-} PersistField a => PersistField [a] where
#else
instance PersistField a => PersistField [a] where
#endif
    toPersistValue = PersistList . fmap toPersistValue
    fromPersistValue (PersistList l) = fromPersistList l
    fromPersistValue (PersistText t) = fromPersistValue (PersistByteString $ TE.encodeUtf8 t)
    fromPersistValue (PersistByteString bs)
        | Just values <- A.decode' (L.fromChunks [bs]) = fromPersistList values
    -- avoid the need for a migration to fill in empty lists.
    -- also useful when Persistent is not the only one filling in the data
    fromPersistValue (PersistNull) = Right []
    fromPersistValue x = Left $ T.pack $ "Expected PersistList, received: " ++ show x

instance PersistField a => PersistField (V.Vector a) where
  toPersistValue = toPersistValue . V.toList
  fromPersistValue = either (\e -> Left ("Vector: " `T.append` e))
                            (Right . V.fromList) . fromPersistValue

instance (Ord a, PersistField a) => PersistField (S.Set a) where
    toPersistValue = PersistList . fmap toPersistValue . S.toList
    fromPersistValue (PersistList list) =
      S.fromList <$> fromPersistList list
    fromPersistValue (PersistText t) = fromPersistValue (PersistByteString $ TE.encodeUtf8 t)
    fromPersistValue (PersistByteString bs)
        | Just values <- A.decode' (L.fromChunks [bs]) =
            S.fromList <$> fromPersistList values
    fromPersistValue PersistNull = Right S.empty
    fromPersistValue x = Left $ T.pack $ "Expected PersistSet, received: " ++ show x

instance (PersistField a, PersistField b) => PersistField (a,b) where
    toPersistValue (x,y) = PersistList [toPersistValue x, toPersistValue y]
    fromPersistValue v =
        case fromPersistValue v of
            Right [x,y]  -> (,) <$> fromPersistValue x <*> fromPersistValue y
            Left e       -> Left e
            _            -> Left $ T.pack $ "Expected 2 item PersistList, received: " ++ show v

instance PersistField v => PersistField (IM.IntMap v) where
    toPersistValue = toPersistValue . IM.toList
    fromPersistValue = fmap IM.fromList . fromPersistValue

instance PersistField v => PersistField (M.Map T.Text v) where
    toPersistValue = PersistMap . fmap (second toPersistValue) . M.toList
    fromPersistValue = fromPersistMap <=< getPersistMap

instance PersistField PersistValue where
    toPersistValue = id
    fromPersistValue = Right

fromPersistList :: PersistField a => [PersistValue] -> Either T.Text [a]
fromPersistList = mapM fromPersistValue

fromPersistMap :: PersistField v
               => [(T.Text, PersistValue)]
               -> Either T.Text (M.Map T.Text v)
fromPersistMap = foldShortLeft fromPersistValue [] where
    -- a fold that short-circuits on Left.
    foldShortLeft f = go
      where
        go acc [] = Right $ M.fromList acc
        go acc ((k, v):kvs) =
          case f v of
            Left e   -> Left e
            Right v' -> go ((k,v'):acc) kvs

-- | FIXME Add documentation to that.
getPersistMap :: PersistValue -> Either T.Text [(T.Text, PersistValue)]
getPersistMap (PersistMap kvs) = Right kvs
getPersistMap (PersistText t)  = getPersistMap (PersistByteString $ TE.encodeUtf8 t)
getPersistMap (PersistByteString bs)
    | Just pairs <- A.decode' (L.fromChunks [bs]) = Right pairs
getPersistMap PersistNull = Right []
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
    fromPersistValue (PersistInt64 1)    = Right Active
    fromPersistValue (PersistByteString i) = case readInt i of
                                               Just (0,"") -> Left $ T.pack "PersistField Checkmark: found unexpected 0 value"
                                               Just (1,"") -> Right Active
                                               xs -> Left $ T.pack $ "PersistField Checkmark failed parsing PersistByteString xs["++show xs++"] i["++show i++"]"
    fromPersistValue (PersistBool False) =
      Left $ T.pack "PersistField Checkmark: found unexpected FALSE value"
    fromPersistValue other =
      Left $ T.pack $ "PersistField Checkmark: unknown value " ++ show other
