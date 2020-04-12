{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JSONTest where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import qualified Data.Vector as V (fromList)
import Test.HUnit (assertBool)
import Test.Hspec.Expectations ()

import Database.Persist
import Database.Persist.Postgresql.JSON

import PgInit


share [mkPersist persistSettings,  mkMigrate "jsonTestMigrate"] [persistLowerCase|
  TestValue
    json Value
    deriving Show
|]

cleanDB :: (BaseBackend backend ~ SqlBackend, PersistQueryWrite backend, MonadIO m)
        => ReaderT backend m ()
cleanDB = deleteWhere ([] :: [Filter TestValue])

emptyArr :: Value
emptyArr = toJSON ([] :: [Value])

insert' :: (MonadIO m, PersistStoreWrite backend, BaseBackend backend ~ SqlBackend)
        => Value -> ReaderT backend m (Key TestValue)
insert' = insert . TestValue


matchKeys :: (Show record, Show (Key record), MonadIO m, Eq (Key record))
          => [Key record] -> [Entity record] -> m ()
matchKeys ys xs = do
  msg1 `assertBoolIO` (xLen == yLen)
  forM_ ys $ \y -> msg2 y `assertBoolIO` (y `elem` ks)
    where ks = entityKey <$> xs
          xLen = length xs
          yLen = length ys
          msg1 = mconcat
              [ "\nexpected: ", show yLen
              , "\n but got: ", show xLen
              , "\n[xs: ", show xs, "]"
              , "\n[ys: ", show ys, "]"
              ]
          msg2 y = mconcat
              [ "key \"", show y
              , "\" not in result:\n  ", show ks
              ]

setup :: IO TestKeys
setup = asIO $ runConn_ $ do
  void $ runMigrationSilent jsonTestMigrate
  testKeys

teardown :: IO ()
teardown = asIO $ runConn_ $ do
    cleanDB

shouldBeIO :: (Show a, Eq a, MonadIO m) => a -> a -> m ()
shouldBeIO x y = liftIO $ shouldBe x y

assertBoolIO :: MonadIO m => String -> Bool -> m ()
assertBoolIO s b = liftIO $ assertBool s b

testKeys :: (Monad m, MonadIO m) => ReaderT SqlBackend m TestKeys
testKeys = do
    nullK <- insert' Null

    boolTK <- insert' $ Bool True
    boolFK <- insert' $ toJSON False

    num0K <- insert' $ Number 0
    num1K <- insert' $ Number 1
    numBigK <- insert' $ toJSON (1234567890 :: Int)
    numFloatK <- insert' $ Number 0.0
    numSmallK <- insert' $ Number 0.0000000000000000123
    numFloat2K <- insert' $ Number 1.5
    -- numBigFloatK will turn into 9876543210.123457 because JSON
    numBigFloatK <- insert' $ toJSON (9876543210.123456789 :: Double)

    strNullK <- insert' $ String ""
    strObjK <- insert' $ String "{}"
    strArrK <- insert' $ String "[]"
    strAK <- insert' $ String "a"
    strTestK <- insert' $ toJSON ("testing" :: Text)
    str2K <- insert' $ String "2"
    strFloatK <- insert' $ String "0.45876"

    arrNullK <- insert' $ Array $ V.fromList []
    arrListK <- insert' $ toJSON [emptyArr,emptyArr,toJSON [emptyArr,emptyArr]]
    arrList2K <- insert' $ toJSON [emptyArr,toJSON [Number 3,Bool False]
                                  ,toJSON [emptyArr,toJSON [Object mempty]]
                                  ]
    arrFilledK <- insert' $ toJSON [Null, Number 4, String "b"
                                   ,Object mempty, emptyArr
                                   ,object [ "test" .= [Null], "test2" .= String "yes"]
                                   ]
    arrList3K <- insert' $ toJSON [toJSON [String "a"], Number 1]
    arrList4K <- insert' $ toJSON [String "a", String "b", String "c", String "d"]

    objNullK <- insert' $ Object mempty
    objTestK <- insert' $ object ["test" .= Null, "test1" .= String "no"]
    objDeepK <- insert' $ object ["c" .= Number 24.986, "foo" .= object ["deep1" .= Bool True]]
    objEmptyK <- insert' $ object ["" .= Number 9001]
    objFullK  <- insert' $ object ["a" .= Number 1, "b" .= Number 2
                                  ,"c" .= Number 3, "d" .= Number 4
                                  ]
    return TestKeys{..}

data TestKeys =
  TestKeys { nullK :: Key TestValue
             , boolTK :: Key TestValue
             , boolFK :: Key TestValue
             , num0K :: Key TestValue
             , num1K :: Key TestValue
             , numBigK :: Key TestValue
             , numFloatK :: Key TestValue
             , numSmallK :: Key TestValue
             , numFloat2K :: Key TestValue
             , numBigFloatK :: Key TestValue
             , strNullK :: Key TestValue
             , strObjK :: Key TestValue
             , strArrK :: Key TestValue
             , strAK :: Key TestValue
             , strTestK :: Key TestValue
             , str2K :: Key TestValue
             , strFloatK :: Key TestValue
             , arrNullK :: Key TestValue
             , arrListK :: Key TestValue
             , arrList2K :: Key TestValue
             , arrFilledK :: Key TestValue
             , objNullK :: Key TestValue
             , objTestK :: Key TestValue
             , objDeepK :: Key TestValue
             , arrList3K  :: Key TestValue
             , arrList4K  :: Key TestValue
             , objEmptyK  :: Key TestValue
             , objFullK   :: Key TestValue
             } deriving (Eq, Ord, Show)

specs :: Spec
specs = afterAll_ teardown $ do
  beforeAll setup $ do
    describe "Testing JSON operators" $ do
      describe "@>. object queries" $ do
        it "matches an empty Object with any object" $
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. Object mempty] []
            [objNullK, objTestK, objDeepK, objEmptyK, objFullK] `matchKeys`  vals

        it "matches a subset of object properties" $
            -- {test: null, test1: no} @>. {test: null} == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. object ["test" .= Null]] []
            [objTestK] `matchKeys`  vals

        it "matches a nested object against an empty object at the same key" $
            -- {c: 24.986, foo: {deep1: true}} @>. {foo: {}} == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. object ["foo" .= object []]] []
            [objDeepK] `matchKeys`  vals

        it "doesn't match a nested object against a string at the same key" $
            -- {c: 24.986, foo: {deep1: true}} @>. {foo: nope} == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. object ["foo" .= String "nope"]] []
            [] `matchKeys`  vals

        it "matches a nested object when the query object is identical" $
            -- {c: 24.986, foo: {deep1: true}} @>. {foo: {deep1: true}} == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. (object ["foo" .= object ["deep1" .= True]])] []
            [objDeepK] `matchKeys`  vals

        it "doesn't match a nested object when queried with that exact object" $
            -- {c: 24.986, foo: {deep1: true}} @>. {deep1: true} == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. object ["deep1" .= True]] []
            [] `matchKeys`  vals

      describe "@>. array queries" $ do
        it "matches an empty Array with any list" $
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. emptyArr] []
            [arrNullK, arrListK, arrList2K, arrFilledK, arrList3K, arrList4K] `matchKeys`  vals

        it "matches list when queried with subset (1 item)" $
            -- [null, 4, 'b', {}, [], {test: [null], test2: 'yes'}] @>. [4] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON [4 :: Int]] []
            [arrFilledK] `matchKeys` vals

        it "matches list when queried with subset (2 items)" $
            -- [null, 4, 'b', {}, [], {test: [null], test2: 'yes'}] @>. [null,'b'] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON [Null, String "b"]] []
            [arrFilledK] `matchKeys` vals

        it "doesn't match list when queried with intersecting list (1 match, 1 diff)" $
            -- [null, 4, 'b', {}, [], {test: [null], test2: 'yes'}] @>. [null,'d'] == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON [emptyArr, String "d"]] []
            [] `matchKeys` vals

        it "matches list when queried with same list in different order" $
            -- [null, 4, 'b', {}, [], {test: [null], test2: 'yes'}] @>.
            -- [[],'b',{test: [null],test2: 'yes'},4,null,{}] == True
          \TestKeys {..} -> runConnAssert $ do
            let queryList =
                  toJSON [ emptyArr, String "b"
                         , object [ "test" .= [Null], "test2" .= String "yes"]
                         , Number 4, Null, Object mempty ]

            vals <- selectList [TestValueJson @>. queryList ] []
            [arrFilledK] `matchKeys` vals

        it "doesn't match list when queried with same list + 1 item" $
            -- [null,4,'b',{},[],{test:[null],test2:'yes'}] @>.
            -- [null,4,'b',{},[],{test:[null],test2: 'yes'}, false] == False
          \TestKeys {..} -> runConnAssert $ do
            let testList =
                  toJSON [ Null, Number 4, String "b", Object mempty, emptyArr
                         , object [ "test" .= [Null], "test2" .= String "yes"]
                         , Bool False ]

            vals <- selectList [TestValueJson @>. testList]  []
            [] `matchKeys` vals

        it "matches list when it shares an empty object with the query list" $
            -- [null,4,'b',{},[],{test: [null],test2: 'yes'}] @>. [{}] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON [Object mempty]] []
            [arrFilledK] `matchKeys` vals

        it "matches list with nested list, when queried with an empty nested list" $
            -- [null,4,'b',{},[],{test:[null],test2:'yes'}] @>. [{test:[]}] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON [object ["test" .= emptyArr]]] []
            [arrFilledK] `matchKeys` vals

        it "doesn't match list with nested list, when queried with a diff. nested list" $
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>.
            -- [{"test1":[null]}]  == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON [object ["test1" .= [Null]]]] []
            [] `matchKeys` vals

        it "matches many nested lists when queried with empty nested list" $
            -- [[],[],[[],[]]]                                  @>. [[]] == True
            -- [[],[3,false],[[],[{}]]]                         @>. [[]] == True
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>. [[]] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON [emptyArr]] []
            [arrListK,arrList2K,arrFilledK, arrList3K] `matchKeys` vals

        it "matches nested list when queried with a subset of that list" $
            -- [[],[3,false],[[],[{}]]] @>. [[3]] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON [[3 :: Int]]] []
            [arrList2K] `matchKeys` vals

        it "doesn't match nested list againts a partial intersection of that list" $
            -- [[],[3,false],[[],[{}]]] @>. [[true,3]] == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON [[Bool True, Number 3]]] []
            [] `matchKeys` vals

        it "matches list when queried with raw number contained in the list" $
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>. 4 == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. Number 4] []
            [arrFilledK] `matchKeys` vals

        it "doesn't match list when queried with raw value not contained in the list" $
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>. 99 == False
          \TestKeys {..} -> runConnAssert $ do
          vals <- selectList [TestValueJson @>. Number 99] []
          [] `matchKeys` vals

        it "matches list when queried with raw string contained in the list" $
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>. "b" == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. String "b"] []
            [arrFilledK, arrList4K] `matchKeys` vals

        it "doesn't match list with empty object when queried with \"{}\" " $
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>. "{}" == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. String "{}"] []
            [strObjK] `matchKeys` vals

        it "doesnt match list with nested object when queried with object (not in list)" $
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>.
            -- {"test":[null],"test2":"yes"} == False
          \TestKeys {..} -> runConnAssert $ do
            let queryObject = object [ "test" .= [Null], "test2" .= String "yes"]
            vals <- selectList [TestValueJson @>. queryObject ] []
            [] `matchKeys` vals

      describe "@>. string queries" $ do
        it "matches identical strings" $
            -- "testing" @>. "testing" == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. String "testing"] []
            [strTestK] `matchKeys` vals

        it "doesnt match case insensitive" $
            -- "testing" @>. "Testing" == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. String "Testing"] []
            [] `matchKeys` vals

        it "doesn't match substrings" $
            -- "testing" @>. "test" == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. String "test"] []
            [] `matchKeys` vals

        it "doesn't match strings with object keys" $
            -- "testing" @>. {"testing":1} == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. object ["testing" .= Number 1]] []
            [] `matchKeys` vals

      describe "@>. number queries" $ do
        it "matches identical numbers" $
            -- 1   @>. 1 == True
            -- [1] @>. 1 == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON (1 :: Int)] []
            [num1K, arrList3K] `matchKeys` vals

        it "matches numbers when queried with float" $
            -- 0 @>. 0.0 == True
            -- 0.0 @>. 0.0 == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON (0.0 :: Double)] []
            [num0K,numFloatK] `matchKeys` vals

        it "does not match numbers when queried with a substring of that number" $
            -- 1234567890 @>. 123456789 == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON (123456789 :: Int)] []
            [] `matchKeys` vals

        it "does not match number when queried with different number" $
            -- 1234567890 @>. 234567890 == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON (234567890 :: Int)] []
            [] `matchKeys` vals

        it "does not match number when queried with string of that number" $
            -- 1 @>. "1" == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. String "1"] []
            [] `matchKeys` vals

        it "does not match number when queried with list of digits" $
            -- 1234567890 @>. [1,2,3,4,5,6,7,8,9,0] == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON ([1,2,3,4,5,6,7,8,9,0] :: [Int])] []
            [] `matchKeys` vals

      describe "@>. boolean queries" $ do
        it "matches identical booleans (True)" $
            -- true @>. true == True
            -- false @>. true == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. toJSON True] []
            [boolTK] `matchKeys` vals

        it "matches identical booleans (False)" $
            -- false @>. false == True
            -- true @>. false == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. Bool False] []
            [boolFK] `matchKeys` vals

        it "does not match boolean with string of boolean" $
            -- true @>. "true" == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. String "true"] []
            [] `matchKeys` vals

      describe "@>. null queries" $ do
        it "matches nulls" $
            -- null @>. null == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. Null] []
            [nullK,arrFilledK] `matchKeys` vals

        it "does not match null with string of null" $
            -- null @>. "null" == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson @>. String "null"] []
            [] `matchKeys` vals


      describe "<@. queries" $ do
        it "matches subobject when queried with superobject" $
            -- {}                         <@. {"test":null,"test1":"no","blabla":[]} == True
            -- {"test":null,"test1":"no"} <@. {"test":null,"test1":"no","blabla":[]} == True
          \TestKeys {..} -> runConnAssert $ do
            let queryObject = object ["test" .= Null
                                     , "test1" .= String "no"
                                     , "blabla" .= emptyArr
                                     ]
            vals <- selectList [TestValueJson <@. queryObject] []
            [objNullK,objTestK] `matchKeys` vals

        it "matches raw values and sublists when queried with superlist" $
            -- []    <@. [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
            -- null  <@. [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
            -- false <@. [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] <@.
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
          \TestKeys {..} -> runConnAssert $ do
            let queryList =
                  toJSON [ Null, Number 4, String "b", Object mempty, emptyArr
                         , object [ "test" .= [Null], "test2" .= String "yes"]
                         , Bool False ]

            vals <- selectList [TestValueJson <@. queryList ] []
            [arrNullK,arrFilledK,boolFK,nullK] `matchKeys` vals

        it "matches identical strings" $
            -- "a" <@. "a" == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson <@. String "a"] []
            [strAK] `matchKeys` vals

        it "matches identical big floats" $
            -- 9876543210.123457 <@ 9876543210.123457 == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson <@. Number 9876543210.123457] []
            [numBigFloatK] `matchKeys` vals

        it "doesn't match different big floats" $
            -- 9876543210.123457 <@. 9876543210.123456789 == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson <@. Number 9876543210.123456789] []
            [] `matchKeys` vals

        it "matches nulls" $
            -- null <@. null == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson <@. Null] []
            [nullK] `matchKeys` vals

      describe "?. queries" $ do
        it "matches top level keys and not the keys of nested objects" $
            -- {"test":null,"test1":"no"}                       ?. "test" == True
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?. "test" == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?. "test"] []
            [objTestK] `matchKeys` vals

        it "doesn't match nested key" $
            -- {"c":24.986,"foo":{"deep1":true"}} ?. "deep1" == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?. "deep1"] []
            [] `matchKeys` vals

        it "matches \"{}\" but not empty object when queried with \"{}\"" $
            -- "{}" ?. "{}" == True
            -- {}   ?. "{}" == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?. "{}"] []
            [strObjK] `matchKeys` vals

        it "matches raw empty str and empty str key when queried with \"\"" $
            ---- {}        ?. "" == False
            ---- ""        ?. "" == True
            ---- {"":9001} ?. "" == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?. ""] []
            [strNullK,objEmptyK] `matchKeys` vals

        it "matches lists containing string value when queried with raw string value" $
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?. "b" == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?. "b"] []
            [arrFilledK,arrList4K,objFullK] `matchKeys` vals

        it "matches lists, objects, and raw values correctly when queried with string" $
            -- [["a"]]                   ?. "a" == False
            -- "a"                       ?. "a" == True
            -- ["a","b","c","d"]         ?. "a" == True
            -- {"a":1,"b":2,"c":3,"d":4} ?. "a" == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?. "a"] []
            [strAK,arrList4K,objFullK] `matchKeys` vals

        it "matches string list but not real list when queried with \"[]\"" $
            -- "[]" ?. "[]" == True
            -- []   ?. "[]" == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?. "[]"] []
            [strArrK] `matchKeys` vals

        it "does not match null when queried with string null" $
            -- null ?. "null" == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?. "null"] []
            [] `matchKeys` vals

        it "does not match bool whe nqueried with string bool" $
            -- true ?. "true" == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?. "true"] []
            [] `matchKeys` vals


      describe "?|. queries" $ do
        it "matches raw vals, lists, objects, and nested objects" $
            -- "a"                                              ?|. ["a","b","c"] == True
            -- [["a"],1]                                        ?|. ["a","b","c"] == False
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?|. ["a","b","c"] == True
            -- ["a","b","c","d"]                                ?|. ["a","b","c"] == True
            -- {"a":1,"b":2,"c":3,"d":4}                        ?|. ["a","b","c"] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?|. ["a","b","c"]] []
            [strAK,arrFilledK,objDeepK,arrList4K,objFullK] `matchKeys` vals

        it "matches str object but not object when queried with \"{}\"" $
            -- "{}"  ?|. ["{}"] == True
            -- {}    ?|. ["{}"] == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?|. ["{}"]] []
            [strObjK] `matchKeys` vals

        it "doesn't match superstrings when queried with substring" $
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?|. ["test"] == False
            -- "testing"                                        ?|. ["test"] == False
            -- {"test":null,"test1":"no"}                       ?|. ["test"] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?|. ["test"]] []
            [objTestK] `matchKeys` vals

        it "doesn't match nested keys" $
            -- {"c":24.986,"foo":{"deep1":true"}} ?|. ["deep1"] == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?|. ["deep1"]] []
            [] `matchKeys` vals

        it "doesn't match anything when queried with empty list" $
            -- ANYTHING ?|. [] == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?|. []] []
            [] `matchKeys` vals

        it "doesn't match raw, non-string, values when queried with strings" $
            -- true ?|. ["true","null","1"] == False
            -- null ?|. ["true","null","1"] == False
            -- 1    ?|. ["true","null","1"] == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?|. ["true","null","1"]] []
            [] `matchKeys` vals

        it "matches string array when queried with \"[]\"" $
            -- []   ?|. ["[]"] == False
            -- "[]" ?|. ["[]"] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?|. ["[]"]] []
            [strArrK] `matchKeys` vals

      describe "?&. queries" $ do
        it "matches anything when queried with an empty list" $
            -- ANYTHING ?&. [] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?&. []] []
            flip matchKeys vals [ nullK
                                , boolTK, boolFK
                                , num0K, num1K, numBigK, numFloatK
                                , numSmallK, numFloat2K, numBigFloatK
                                , strNullK, strObjK, strArrK, strAK
                                , strTestK, str2K, strFloatK
                                , arrNullK, arrListK, arrList2K
                                , arrFilledK, arrList3K, arrList4K
                                , objNullK, objTestK, objDeepK
                                , objEmptyK, objFullK
                                ]

        it "matches raw values, lists, and objects when queried with string" $
            -- "a"                       ?&. ["a"] == True
            -- [["a"],1]                 ?&. ["a"] == False
            -- ["a","b","c","d"]         ?&. ["a"] == True
            -- {"a":1,"b":2,"c":3,"d":4} ?&. ["a"] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?&. ["a"]] []
            [strAK,arrList4K,objFullK] `matchKeys` vals

        it "matches raw values, lists, and objects when queried with multiple string" $
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?&. ["b","c"] == False
            -- {"c":24.986,"foo":{"deep1":true"}}               ?&. ["b","c"] == False
            -- ["a","b","c","d"]                                ?&. ["b","c"] == True
            -- {"a":1,"b":2,"c":3,"d":4}                        ?&. ["b","c"] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?&. ["b","c"]] []
            [arrList4K,objFullK] `matchKeys` vals

        it "matches object string when queried with \"{}\"" $
            -- {}   ?&. ["{}"] == False
            -- "{}" ?&. ["{}"] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?&. ["{}"]] []
            [strObjK] `matchKeys` vals

        it "doesn't match superstrings when queried with substring" $
            -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?&. ["test"] == False
            -- "testing"                                        ?&. ["test"] == False
            -- {"test":null,"test1":"no"}                       ?&. ["test"] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?&. ["test"]] []
            [objTestK] `matchKeys` vals

        it "doesn't match nested keys" $
            -- {"c":24.986,"foo":{"deep1":true"}} ?&. ["deep1"] == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?&. ["deep1"]] []
            [] `matchKeys` vals

        it "doesn't match anything when there is a partial match" $
            -- "a"                       ?&. ["a","e"] == False
            -- ["a","b","c","d"]         ?&. ["a","e"] == False
            -- {"a":1,"b":2,"c":3,"d":4} ?&. ["a","e"] == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?&. ["a","e"]] []
            [] `matchKeys` vals

        it "matches string array when queried with \"[]\"" $
            -- []   ?&. ["[]"] == False
            -- "[]" ?&. ["[]"] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?&. ["[]"]] []
            [strArrK] `matchKeys` vals

        it "doesn't match null when queried with string null" $
            -- THIS WILL FAIL IF THE IMPLEMENTATION USES
            -- @ '{null}' @
            -- INSTEAD OF
            -- @ ARRAY['null'] @
            -- null ?&. ["null"] == False
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?&. ["null"]] []
            [] `matchKeys` vals

        it "doesn't match number when queried with str of that number" $
            -- [["a"],1] ?&. ["1"] == False
            -- "1"       ?&. ["1"] == True
          \TestKeys {..} -> runConnAssert $ do
          str1 <- insert' $ toJSON $ String "1"
          vals <- selectList [TestValueJson ?&. ["1"]] []
          [str1] `matchKeys` vals

        it "doesn't match empty objs or list when queried with empty string" $
            -- {}        ?&. [""] == False
            -- []        ?&. [""] == False
            -- ""        ?&. [""] == True
            -- {"":9001} ?&. [""] == True
          \TestKeys {..} -> runConnAssert $ do
            vals <- selectList [TestValueJson ?&. [""]] []
            [strNullK,objEmptyK] `matchKeys` vals
