{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Instances where

import Database.Persist.Sql
import Data.UUID (UUID, fromASCIIBytes, toASCIIBytes, toText, fromText)
import Web.PathPieces (PathPiece(..))
import qualified Data.Text as T
import qualified Data.Aeson as J
import Data.Aeson (Value(..))
import qualified Data.Text.Encoding as TE

instance PersistField UUID where
  toPersistValue = PersistDbSpecific . toASCIIBytes
  fromPersistValue (PersistDbSpecific uuid) =
    case fromASCIIBytes uuid of
      Nothing -> Left $ "Model/CustomTypes.hs: Failed to deserialize a UUID; received: " <> T.pack (show uuid)
      Just uuid' -> Right uuid'
  fromPersistValue x = Left $ "Model/CustomTypes.hs: When trying to deserialize a UUID: expected PersistDbSpecific, received: " <> (T.pack $ show x)

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  toPathPiece = toText
  fromPathPiece = fromText

instance PersistField Value where
  toPersistValue value = PersistText $ TE.encodeUtf8 $ J.encode value
  fromPersistValue (PersistText t) = case J.eitherDecode (cs t) of
    Left s -> Left $ "Error decoding into Value; received " ++ t ++ " error: " ++ T.pack s
    Right v -> Right v
  fromPersistValue (PersistByteString bs) = case J.eitherDecode (TE.encodeUtf8 bs) of
    Left s -> Left $ "Error decoding into Value; received " ++ TE.encodeUtf8 bs ++ " error: " ++ T.pack s
    Right v -> Right v
  fromPersistValue _x = Left . T.pack $ "Value: When expecting PersistByteString/PersistText, received: " ++ show x

instance PersistFieldSql Value where
  sqlType _ = SqlOther "jsonb"