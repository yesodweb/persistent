{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Filter operators for JSON values added to PostgreSQL 9.4
module Database.Persist.Postgresql.JSON
  ( (@>.)
  , (<@.)
  , (?.)
  , (?|.)
  , (?&.)
  , Value()
  ) where

import Data.Aeson (FromJSON, ToJSON, Value, encode, eitherDecodeStrict)
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as TE (encodeUtf8)

import Database.Persist (EntityField, Filter(..), PersistValue(..), PersistField(..), PersistFilter(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlType(..))
import Database.Persist.Types (FilterValue(..))


infix 4 @>., <@., ?., ?|., ?&.

-- | This operator checks inclusion of the JSON value
-- on the right hand side in the JSON value on the left
-- hand side.
--
-- === __Objects__
--
-- An empty Object matches any object
--
-- @
-- {}                \@> {} == True
-- {"a":1,"b":false} \@> {} == True
-- @
--
-- Any key-value will be matched top-level
--
-- @
-- {"a":1,"b":{"c":true"}} \@> {"a":1}         == True
-- {"a":1,"b":{"c":true"}} \@> {"b":1}         == False
-- {"a":1,"b":{"c":true"}} \@> {"b":{}}        == True
-- {"a":1,"b":{"c":true"}} \@> {"c":true}      == False
-- {"a":1,"b":{"c":true"}} \@> {"b":{c":true}} == True
-- @
--
-- === __Arrays__
--
-- An empty Array matches any array
--
-- @
-- []                    \@> [] == True
-- [1,2,"hi",false,null] \@> [] == True
-- @
--
-- Any array has to be a sub-set.
-- Any object or array will also be compared as being a subset of.
--
-- @
-- [1,2,"hi",false,null] \@> [1]                       == True
-- [1,2,"hi",false,null] \@> [null,"hi"]               == True
-- [1,2,"hi",false,null] \@> ["hi",true]               == False
-- [1,2,"hi",false,null] \@> ["hi",2,null,false,1]     == True
-- [1,2,"hi",false,null] \@> [1,2,"hi",false,null,{}]  == False
-- @
--
-- Arrays and objects inside arrays match the same way they'd
-- be matched as being on their own.
--
-- @
-- [1,"hi",[false,3],{"a":[null]}] \@> [{}]            == True
-- [1,"hi",[false,3],{"a":[null]}] \@> [{"a":[]}]      == True
-- [1,"hi",[false,3],{"a":[null]}] \@> [{"b":[null]}]  == False
-- [1,"hi",[false,3],{"a":[null]}] \@> [[]]            == True
-- [1,"hi",[false,3],{"a":[null]}] \@> [[3]]           == True
-- [1,"hi",[false,3],{"a":[null]}] \@> [[true,3]]      == False
-- @
--
-- A regular value has to be a member
--
-- @
-- [1,2,"hi",false,null] \@> 1      == True
-- [1,2,"hi",false,null] \@> 5      == False
-- [1,2,"hi",false,null] \@> "hi"   == True
-- [1,2,"hi",false,null] \@> false  == True
-- [1,2,"hi",false,null] \@> "2"    == False
-- @
--
-- An object will never match with an array
--
-- @
-- [1,2,"hi",[false,3],{"a":null}] \@> {}          == False
-- [1,2,"hi",[false,3],{"a":null}] \@> {"a":null}  == False
-- @
--
-- === __Other values__
--
-- For any other JSON values the `(\@>.)` operator
-- functions like an equivalence operator.
--
-- @
-- "hello" \@> "hello"     == True
-- "hello" \@> \"Hello"     == False
-- "hello" \@> "h"         == False
-- "hello" \@> {"hello":1} == False
-- "hello" \@> ["hello"]   == False
--
-- 5       \@> 5       == True
-- 5       \@> 5.00    == True
-- 5       \@> 1       == False
-- 5       \@> 7       == False
-- 12345   \@> 1234    == False
-- 12345   \@> 2345    == False
-- 12345   \@> "12345" == False
-- 12345   \@> [1,2,3,4,5] == False
--
-- true    \@> true    == True
-- true    \@> false   == False
-- false   \@> true    == False
-- true    \@> "true"  == False
--
-- null    \@> null    == True
-- null    \@> 23      == False
-- null    \@> "null"  == False
-- null    \@> {}      == False
-- @
--
-- @since 2.8.2
(@>.) :: EntityField record Value -> Value -> Filter record
(@>.) field val = Filter field (FilterValue val) $ BackendSpecificFilter " @> "

-- | Same as '@>.' except the inclusion check is reversed.
-- i.e. is the JSON value on the left hand side included
-- in the JSON value of the right hand side.
--
-- @since 2.8.2
(<@.) :: EntityField record Value -> Value -> Filter record
(<@.) field val = Filter field (FilterValue val) $ BackendSpecificFilter " <@ "

-- | This operator takes a column and a string to find a
-- top-level key/field in an object.
--
-- @column ?. string@
--
-- N.B. This operator might have some unexpected interactions
-- with non-object values. Please reference the examples.
--
-- === __Objects__
--
-- @
-- {"a":null}             ? "a"  == True
-- {"test":false,"a":500} ? "a"  == True
-- {"b":{"a":[]}}         ? "a"  == False
-- {}                     ? "a"  == False
-- {}                     ? "{}" == False
-- {}                     ? ""   == False
-- {"":9001}              ? ""   == True
-- @
--
-- === __Arrays__
--
-- This operator will match an array if the string to be matched
-- is an element of that array, but nothing else.
--
-- @
-- ["a"]              ? "a"   == True
-- [["a"]]            ? "a"   == False
-- [9,false,"1",null] ? "1"   == True
-- []                 ? "[]"  == False
-- [{"a":true}]       ? "a"   == False
-- @
--
-- === __Other values__
--
-- This operator functions like an equivalence operator on strings only.
-- Any other value does not match.
--
-- @
-- "a"  ? "a"    == True
-- "1"  ? "1"    == True
-- "ab" ? "a"    == False
-- 1    ? "1"    == False
-- null ? "null" == False
-- true ? "true" == False
-- 1.5  ? "1.5"  == False
-- @
--
-- @since 2.10.0
(?.) :: EntityField record Value -> Text -> Filter record
(?.) = jsonFilter " ?? "

-- | This operator takes a column and a list of strings to
-- test whether ANY of the elements of the list are top
-- level fields in an object.
--
-- @column ?|. list@
--
-- /N.B. An empty list __will never match anything__. Also, this/
-- /operator might have some unexpected interactions with/
-- /non-object values. Please reference the examples./
--
-- === __Objects__
--
-- @
-- {"a":null}                 ?| ["a","b","c"] == True
-- {"test":false,"a":500}     ?| ["a","b","c"] == True
-- {}                         ?| ["a","{}"]    == False
-- {"b":{"a":[]}}             ?| ["a","c"]     == False
-- {"b":{"a":[]},"test":null} ?| []            == False
-- @
--
-- === __Arrays__
--
-- This operator will match an array if __any__ of the elements
-- of the list are matching string elements of the array.
--
-- @
-- ["a"]              ?| ["a","b","c"] == True
-- [["a"]]            ?| ["a","b","c"] == False
-- [9,false,"1",null] ?| ["a","false"] == False
-- []                 ?| ["a","b","c"] == False
-- []                 ?| []            == False
-- [{"a":true}]       ?| ["a","b","c"] == False
-- [null,4,"b",[]]    ?| ["a","b","c"] == True
-- @
--
-- === __Other values__
--
-- This operator functions much like an equivalence operator
-- on strings only. If a string matches with __any__ element of
-- the given list, the comparison matches. No other values match.
--
-- @
-- "a"  ?| ["a","b","c"] == True
-- "1"  ?| ["a","b","1"] == True
-- "ab" ?| ["a","b","c"] == False
-- 1    ?| ["a","1"]     == False
-- null ?| ["a","null"]  == False
-- true ?| ["a","true"]  == False
-- "a"  ?| []            == False
-- @
--
-- @since 2.10.0
(?|.) :: EntityField record Value -> [Text] -> Filter record
(?|.) field = jsonFilter " ??| " field . PostgresArray

-- | This operator takes a column and a list of strings to
-- test whether ALL of the elements of the list are top
-- level fields in an object.
--
-- @column ?&. list@
--
-- /N.B. An empty list __will match anything__. Also, this/
-- /operator might have some unexpected interactions with/
-- /non-object values. Please reference the examples./
--
-- === __Objects__
--
-- @
-- {"a":null}                 ?& ["a"]         == True
-- {"a":null}                 ?& ["a","a"]     == True
-- {"test":false,"a":500}     ?& ["a"]         == True
-- {"test":false,"a":500}     ?& ["a","b"]     == False
-- {}                         ?& ["{}"]        == False
-- {"b":{"a":[]}}             ?& ["a"]         == False
-- {"b":{"a":[]},"c":false}   ?& ["a","c"]     == False
-- {"a":1,"b":2,"c":3,"d":4}  ?& ["b","d"]     == True
-- {}                         ?& []            == True
-- {"b":{"a":[]},"test":null} ?& []            == True
-- @
--
-- === __Arrays__
--
-- This operator will match an array if __all__ of the elements
-- of the list are matching string elements of the array.
--
-- @
-- ["a"]                   ?& ["a"]         == True
-- ["a"]                   ?& ["a","a"]     == True
-- [["a"]]                 ?& ["a"]         == False
-- ["a","b","c"]           ?& ["a","b","d"] == False
-- [9,"false","1",null]    ?& ["1","false"] == True
-- []                      ?& ["a","b"]     == False
-- [{"a":true}]            ?& ["a"]         == False
-- ["a","b","c","d"]       ?& ["b","c","d"] == True
-- [null,4,{"test":false}] ?& []            == True
-- []                      ?& []            == True
-- @
--
-- === __Other values__
--
-- This operator functions much like an equivalence operator
-- on strings only. If a string matches with all elements of
-- the given list, the comparison matches.
--
-- @
-- "a"   ?& ["a"]     == True
-- "1"   ?& ["a","1"] == False
-- "b"   ?& ["b","b"] == True
-- "ab"  ?& ["a","b"] == False
-- 1     ?& ["1"]     == False
-- null  ?& ["null"]  == False
-- true  ?& ["true"]  == False
-- 31337 ?& []        == True
-- true  ?& []        == True
-- null  ?& []        == True
-- @
--
-- @since 2.10.0
(?&.) :: EntityField record Value -> [Text] -> Filter record
(?&.) field = jsonFilter " ??& " field . PostgresArray

jsonFilter :: PersistField a => Text -> EntityField record Value -> a -> Filter record
jsonFilter op field a = Filter field (UnsafeValue a) $ BackendSpecificFilter op


-----------------
-- AESON VALUE --
-----------------

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
fromPersistValueJsonB x = Left $ fromPersistValueError "FromJSON" "string or bytea" x

-- Constraints on the type might not be necessary,
-- but better to leave them in.
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

newtype PostgresArray a = PostgresArray [a]

instance PersistField a => PersistField (PostgresArray a) where
  toPersistValue (PostgresArray ts) = PersistArray $ toPersistValue <$> ts
  fromPersistValue (PersistArray as) = PostgresArray <$> traverse fromPersistValue as
  fromPersistValue wat = Left $ fromPersistValueError "PostgresArray" "array" wat
