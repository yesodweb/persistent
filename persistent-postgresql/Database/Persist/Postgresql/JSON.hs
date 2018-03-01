{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Postgresql.JSON
  ( (@>.)
  , (<@.)
  , Value()
  ) where

import Data.Aeson (FromJSON, ToJSON, Value, encode, eitherDecodeStrict)
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy)
import Data.Text as T (Text, pack, concat)
import Data.Text.Encoding as TE (encodeUtf8)

import Database.Persist (EntityField, Filter(..), PersistValue(..), PersistField(..), PersistFilter(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlType(..))


infix 4 @>., <@.

-- | This operator checks inclusion of the JSON value
-- on the right hand side in the JSON value on the left
-- hand side.
--
-- Any empty Object matches any object
-- @
-- {"a":1,"b":false} @> {} --> True
-- @
--
-- Any key-value will be matched top-level
-- @
-- {"a":1,"b":{"c:true"}} @> {"a":1}         --> True
-- {"a":1,"b":{"c:true"}} @> {"b":1}         --> False
-- {"a":1,"b":{"c:true"}} @> {"b":{}}        --> True
-- {"a":1,"b":{"c:true"}} @> {"c":true}      --> False
-- {"a":1,"b":{"c:true"}} @> {"b":{c":true}} --> True
-- @
--
-- Any empty Array matches any array
-- @
-- [1,2,"hi",false,null] @> [] --> True
-- @
--
-- Any array has to be a sub-set
-- @
-- [1,2,"hi",false,null] @> [1]                   --> True
-- [1,2,"hi",false,null] @> [null,"hi"]           --> True
-- [1,2,"hi",false,null] @> ["hi",true]           --> False
-- [1,2,"hi",false,null] @> ["hi",2,null,false,1] --> True
-- [1,2,"hi",false,null] @> [3,2,"hi"]            --> False
-- @
--
-- For any other JSON values the `(@>.)` operator
-- functions like an equivalence operator.
-- @
-- "hello" @> "hello" --> True
-- "hello" @> "Hello" --> False
-- "hello" @> "h"     --> False
-- "hello" @> {"a":1} --> False
-- "hello" @> []      --> False
--
-- 5       @> 5       --> True
-- 5       @> 5.00    --> True
-- 5       @> 1       --> False
-- 5       @> 7       --> False
-- 12345   @> 1234    --> False
-- 12345   @> 2345    --> False
-- 12345   @> "12345" --> False
-- 12345   @> [1,2,3,4,5] --> False
--
-- true    @> true    --> True
-- true    @> false   --> False
-- false   @> true    --> False
-- true    @> "true"  --> False
--
-- null    @> null    --> True
-- null    @> 23      --> False
-- null    @> "null"  --> False
-- null    @> {}      --> False
-- @
(@>.) :: EntityField record Value -> Value -> Filter record
(@>.) field val = Filter field (Left val) $ BackendSpecificFilter " @> "

-- | Same as `(<@.)` except the inclusion check is reversed.
-- i.e. is the JSON value on the left hand side included
-- in the JSON value of the right hand side.
(<@.) :: EntityField record Value -> Value -> Filter record
(<@.) field val = Filter field (Left val) $ BackendSpecificFilter " <@ "


instance PersistField Value where
  toPersistValue = toPersistValueJsonB
  fromPersistValue = fromPersistValueJsonB

instance PersistFieldSql Value where
  sqlType = sqlTypeJsonB

-- FIXME: PersistText might be a bit more efficient,
-- but needs testing/profiling before changing it.
-- (When entering into the DB the type isn't as important as fromPersistValue)
toPersistValueJsonB :: ToJSON a => a -> PersistValue
toPersistValueJsonB = PersistDbSpecific . BSL.toStrict . encode

fromPersistValueJsonB :: FromJSON a => PersistValue -> Either Text a
fromPersistValueJsonB (PersistText t) =
    case eitherDecodeStrict $ TE.encodeUtf8 t of
      Left str -> Left $ fromPersistValueParseError "FromJSON" t $ T.pack str
      Right v -> Right v
fromPersistValueJsonB (PersistByteString bs) =
    case eitherDecodeStrict bs of
      Left str -> Left $ fromPersistValueParseError "FromJSON" bs $ T.pack str
      Right v -> Right v
fromPersistValueJsonB x = Left $ fromPersistValueError "FromJSON" "string or bytestring" x

-- Constraints on the type are not necessary.
sqlTypeJsonB :: (ToJSON a, FromJSON a) => Proxy a -> SqlType
sqlTypeJsonB _ = SqlOther "JSONB"


fromPersistValueError :: Text -- ^ Haskell type, should match Haskell name exactly, e.g. "Int64"
                      -> Text -- ^ Database type(s), should appear different from Haskell name, e.g. "integer" or "INT", not "Int".
                      -> PersistValue -- ^ Incorrect value
                      -> Text -- ^ Error message
fromPersistValueError haskellType databaseType received = T.concat
    [ "Failed to parse Haskell type `"
    , haskellType
    , "`; expected "
    , databaseType
    , " from database, but received: "
    , T.pack (show received)
    , ". Potential solution: Check that your database schema matches your Persistent model definitions."
    ]

fromPersistValueParseError :: (Show a)
                           => Text -- ^ Haskell type, should match Haskell name exactly, e.g. "Int64"
                           -> a -- ^ Received value
                           -> Text -- ^ Additional error
                           -> Text -- ^ Error message
fromPersistValueParseError haskellType received err = T.concat
    [ "Failed to parse Haskell type `"
    , haskellType
    , "`, but received "
    , T.pack (show received)
    , " | with error: "
    , err
    ]
