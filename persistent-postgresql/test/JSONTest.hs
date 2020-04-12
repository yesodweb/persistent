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

cleanDB :: (BaseBackend backend ~ SqlBackend, PersistQueryWrite backend, MonadIO m) => ReaderT backend m ()
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

setup :: IO ()
setup = asIO $ runConn $ do
    void $ runMigrationSilent jsonTestMigrate

teardown :: IO ()
teardown = asIO $ runConn $ do
  cleanDB

shouldBeIO :: (Show a, Eq a, MonadIO m) => a -> a -> m ()
shouldBeIO x y = liftIO $ shouldBe x y

assertBoolIO :: MonadIO m => String -> Bool -> m ()
assertBoolIO s b = liftIO $ assertBool s b

edgeCases :: (Monad m, MonadIO m) => ReaderT SqlBackend m (EdgeCaseKeys TestValue)
edgeCases = do
  arrList3K <- insert' $ toJSON [toJSON [String "a"], Number 1]
  arrList4K <- insert' $ toJSON [String "a", String "b", String "c", String "d"]
  objEmptyK <- insert' $ object ["" .= Number 9001]
  objFullK  <- insert' $ object ["a" .= Number 1, "b" .= Number 2
                                , "c" .= Number 3, "d" .= Number 4 ]
  return $ EdgeCaseKeys {..}

globalKeys :: (Monad m, MonadIO m) => ReaderT SqlBackend m (GlobalKeys TestValue)
globalKeys = do
          nullK <- insert' Null
          let nulls = [nullK]

          boolTK <- insert' $ Bool True
          boolFK <- insert' $ toJSON False
          let bools = [boolTK, boolFK]

          num0K <- insert' $ Number 0
          num1K <- insert' $ Number 1
          numBigK <- insert' $ toJSON (1234567890 :: Int)
          numFloatK <- insert' $ Number 0.0
          numSmallK <- insert' $ Number 0.0000000000000000123
          numFloat2K <- insert' $ Number 1.5
          -- numBigFloatK will turn into 9876543210.123457 because JSON
          numBigFloatK <- insert' $ toJSON (9876543210.123456789 :: Double)
          let numbers = [ num0K, num1K, numBigK, numFloatK
                        , numSmallK, numFloat2K, numBigFloatK ]

          strNullK <- insert' $ String ""
          strObjK <- insert' $ String "{}"
          strArrK <- insert' $ String "[]"
          strAK <- insert' $ String "a"
          strTestK <- insert' $ toJSON ("testing" :: Text)
          str2K <- insert' $ String "2"
          strFloatK <- insert' $ String "0.45876"
          let strings = [ strNullK, strObjK, strArrK, strAK
                        , strTestK, str2K, strFloatK ]

          arrNullK <- insert' $ Array $ V.fromList []
          arrListK <- insert' $ toJSON [emptyArr,emptyArr,toJSON [emptyArr,emptyArr]]
          arrList2K <- insert' $ toJSON [emptyArr,toJSON [Number 3,Bool False]
                                        ,toJSON [emptyArr,toJSON [Object mempty]]
                                        ]
          arrFilledK <- insert' $ toJSON [Null, Number 4, String "b"
                                         ,Object mempty, emptyArr
                                         ,object [ "test" .= [Null], "test2" .= String "yes"]]
          let lists = [ arrNullK, arrListK, arrList2K, arrFilledK ]

          objNullK <- insert' $ Object mempty
          objTestK <- insert' $ object ["test" .= Null, "test1" .= String "no"]
          objDeepK <- insert' $ object ["c" .= Number 24.986, "foo" .= object ["deep1" .= Bool True]]
          let objects = [ objNullK, objTestK, objDeepK ]
          return GlobalKeys{..}


data EdgeCaseKeys record =
  EdgeCaseKeys { arrList3K  :: Key record
               , arrList4K  :: Key record
               , objEmptyK  :: Key record
               , objFullK   :: Key record }

data GlobalKeys record =
  GlobalKeys { nulls   :: [Key record]
             , bools   :: [Key record]
             , numbers :: [Key record]
             , strings :: [Key record]
             , lists   :: [Key record]
             , objects :: [Key record] }

specs :: Spec
specs =
    describe "Testing JSON operators"
    $ beforeAll setup
    $ afterAll_ teardown $ do

      -- Setup DB and get ahold of keys
      GlobalKeys {..} <- runIO $ runConn_ $ globalKeys
      let [ nullK ] = nulls
      let [ boolTK, boolFK ] = bools
      let [ num0K, num1K, numBigK, numFloatK, numSmallK, numFloat2K, numBigFloatK ] = numbers
      let [ strNullK, strObjK, strArrK, strAK, strTestK, str2K, strFloatK ] = strings
      let [ arrNullK, arrListK, arrList2K, arrFilledK ] = lists
      let [ objNullK, objTestK, objDeepK ] = objects

      describe "@>. object queries" $ do
        it "matches an empty Object with any object" $ db $ do
            vals <- selectList [TestValueJson @>. Object mempty] []
            objects `matchKeys`  vals

        it "matches a subset of object properties" $ db $ do
            -- {test: null, test1: no} @>. {test: null} == True
            vals <- selectList [TestValueJson @>. object ["test" .= Null]] []
            [objTestK] `matchKeys`  vals

        it "matches a nested object against an empty object at the same key" $ db $ do
            -- {c: 24.986, foo: {deep1: true}} @>. {foo: {}} == True
            vals <- selectList [TestValueJson @>. object ["foo" .= object []]] []
            [objDeepK] `matchKeys`  vals

        it "doesn't match a nested object against a string at the same key" $ db $ do
            -- {c: 24.986, foo: {deep1: true}} @>. {foo: nope} == False
            vals <- selectList [TestValueJson @>. object ["foo" .= String "nope"]] []
            [] `matchKeys`  vals

        it "matches a nested object when the query object is identical" $ db $ do
            -- {c: 24.986, foo: {deep1: true}} @>. {foo: {deep1: true}} == True
            vals <- selectList [TestValueJson @>. (object ["foo" .= object ["deep1" .= True]])] []
            [objDeepK] `matchKeys`  vals

        it "doesn't match a nested object when queried with that exact object" $ db $ do
            -- {c: 24.986, foo: {deep1: true}} @>. {deep1: true} == False
            vals <- selectList [TestValueJson @>. object ["deep1" .= True]] []
            [] `matchKeys`  vals

      describe "@>. array queries" $ do
        it "matches an empty Array with any list" $ db $ do
            vals <- selectList [TestValueJson @>. emptyArr] []
            lists `matchKeys`  vals

        it "matches list when queried with subset (1 item)" $ db $ do
            -- [null, 4, 'b', {}, [], {test: [null], test2: 'yes'}] @>. [4] == True
            vals <- selectList [TestValueJson @>. toJSON [4 :: Int]] []
            [arrFilledK] `matchKeys` vals

        it "matches list when queried with subset (2 items)" $ db $ do
            -- [null, 4, 'b', {}, [], {test: [null], test2: 'yes'}] @>. [null,'b'] == True
            vals <- selectList [TestValueJson @>. toJSON [Null, String "b"]] []
            [arrFilledK] `matchKeys` vals

        it "doesn't match list when queried with intersecting list (1 match, 1 diff)" $ db $ do
            -- [null, 4, 'b', {}, [], {test: [null], test2: 'yes'}] @>. [null,'d'] == False
            vals <- selectList [TestValueJson @>. toJSON [emptyArr, String "d"]] []
            [] `matchKeys` vals

        it "matches list when queried with same list in different order" $ db $ do
            -- [null, 4, 'b', {}, [], {test: [null], test2: 'yes'}] @>.
            -- [[],'b',{test: [null],test2: 'yes'},4,null,{}] == True
            let queryList =
                  toJSON [ emptyArr, String "b"
                         , object [ "test" .= [Null], "test2" .= String "yes"]
                         , Number 4, Null, Object mempty ]

            vals <- selectList [TestValueJson @>. queryList ] []
            [arrFilledK] `matchKeys` vals

        it "doesn't match list when queried with same list + 1 item" $ db $ do
            -- [null,4,'b',{},[],{test:[null],test2:'yes'}] @>.
            -- [null,4,'b',{},[],{test:[null],test2: 'yes'}, false] == False
            let testList =
                  toJSON [ Null, Number 4, String "b", Object mempty, emptyArr
                         , object [ "test" .= [Null], "test2" .= String "yes"]
                         , Bool False ]

            vals <- selectList [TestValueJson @>. testList]  []
            [] `matchKeys` vals

        it "matches list when it shares an empty object with the query list" $ db $ do
            -- [null,4,'b',{},[],{test: [null],test2: 'yes'}] @>. [{}] == True
            vals <- selectList [TestValueJson @>. toJSON [Object mempty]] []
            [arrFilledK] `matchKeys` vals

        it "matches list with nested list, when queried with an empty nested list" $ db $ do
            -- [null,4,'b',{},[],{test:[null],test2:'yes'}] @>. [{test:[]}] == True
            vals <- selectList [TestValueJson @>. toJSON [object ["test" .= emptyArr]]] []
            [arrFilledK] `matchKeys` vals

        it "doesn't match list with nested list, when queried with a diff. nested list" $ db $ do
          -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>.
          -- [{"test1":[null]}]  == False
          vals <- selectList [TestValueJson @>. toJSON [object ["test1" .= [Null]]]] []
          [] `matchKeys` vals

        it "matches many nested lists when queried with empty nested list" $ db $ do
          ---- [[],[],[[],[]]]                                  @>. [[]] == True
          ---- [[],[3,false],[[],[{}]]]                         @>. [[]] == True
          ---- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>. [[]] == True
          vals <- selectList [TestValueJson @>. toJSON [emptyArr]] []
          [arrListK,arrList2K,arrFilledK] `matchKeys` vals

        it "matches nested list when queried with a subset of that list" $ db $ do
          ---- [[],[3,false],[[],[{}]]] @>. [[3]] == True
          vals <- selectList [TestValueJson @>. toJSON [[3 :: Int]]] []
          [arrList2K] `matchKeys` vals

        it "doesn't match nested list againts a partial intersection of that list" $ db $ do
          ---- [[],[3,false],[[],[{}]]] @>. [[true,3]] == False
          vals <- selectList [TestValueJson @>. toJSON [[Bool True, Number 3]]] []
          [] `matchKeys` vals

        it "matches list when queried with raw number contained in the list" $ db $ do
          ---- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>. 4 == True
          vals <- selectList [TestValueJson @>. Number 4] []
          [arrFilledK] `matchKeys` vals

        it "doesn't match list when queried with raw value not contained in the list" $ db $ do
          ---- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>. 99 == False
          vals <- selectList [TestValueJson @>. Number 99] []
          [] `matchKeys` vals

        it "matches list when queried with raw string contained in the list" $ db $ do
          ---- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>. "b" == True
          vals <- selectList [TestValueJson @>. String "b"] []
          [arrFilledK] `matchKeys` vals

        it "doesn't match list with empty object when queried with \"{}\" " $ db $ do
          -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>. "{}" == False
          vals <- selectList [TestValueJson @>. String "{}"] []
          [strObjK] `matchKeys` vals

        it "doesnt match list with nested object when queried with object (not in list)" $ db $ do
          -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @>.
          --{"test":[null],"test2":"yes"} == False
          let queryObject = object [ "test" .= [Null], "test2" .= String "yes"]
          vals <- selectList [TestValueJson @>. queryObject ] []
          [] `matchKeys` vals

      describe "@>. string queries" $ do
        it "matches identical strings" $ db $ do
          -- "testing" @>. "testing" == True
          vals <- selectList [TestValueJson @>. String "testing"] []
          [strTestK] `matchKeys` vals

        it "doesnt match case insensitive" $ db $ do
          -- "testing" @>. "Testing" == False
          vals <- selectList [TestValueJson @>. String "Testing"] []
          [] `matchKeys` vals

        it "doesn't match substrings" $ db $ do
          -- "testing" @>. "test" == False
          vals <- selectList [TestValueJson @>. String "test"] []
          [] `matchKeys` vals

        it "doesn't match strings with object keys" $ db $ do
          -- "testing" @>. {"testing":1} == False
          vals <- selectList [TestValueJson @>. object ["testing" .= Number 1]] []
          [] `matchKeys` vals

      describe "@>. number queries" $ do

        it "matches identical numbers" $ db $ do
          -- 1 @>. 1 == True
          vals <- selectList [TestValueJson @>. toJSON (1 :: Int)] []
          [num1K] `matchKeys` vals

        it "matches numbers when queried with float" $ db $ do
          -- 0 @>. 0.0 == True
          -- 0.0 @>. 0.0 == True
          vals <- selectList [TestValueJson @>. toJSON (0.0 :: Double)] []
          [num0K,numFloatK] `matchKeys` vals

        it "does not match numbers when queried with a substring of that number" $ db $ do
          -- 1234567890 @>. 123456789 == False
          vals <- selectList [TestValueJson @>. toJSON (123456789 :: Int)] []
          [] `matchKeys` vals

        it "does not match number when queried with different number" $ db $ do
          -- 1234567890 @>. 234567890 == False
          vals <- selectList [TestValueJson @>. toJSON (234567890 :: Int)] []
          [] `matchKeys` vals

        it "does not match number when queried with string of that number" $ db $ do
          -- 1 @>. "1" == False
          vals <- selectList [TestValueJson @>. String "1"] []
          [] `matchKeys` vals

        it "does not match number when queried with list of digits" $ db $ do
          -- 1234567890 @>. [1,2,3,4,5,6,7,8,9,0] == False
          vals <- selectList [TestValueJson @>. toJSON ([1,2,3,4,5,6,7,8,9,0] :: [Int])] []
          [] `matchKeys` vals

      describe "@>. boolean queries" $ do

        it "matches identical booleans (True)" $ db $ do
          -- true @>. true == True
          -- false @>. true == False
          vals <- selectList [TestValueJson @>. toJSON True] []
          [boolTK] `matchKeys` vals

        it "matches identical booleans (False)" $ db $ do
          -- false @>. false == True
          -- true @>. false == False
          vals <- selectList [TestValueJson @>. Bool False] []
          [boolFK] `matchKeys` vals

        it "does not match boolean with string of boolean" $ db $ do
          -- true @>. "true" == False
          vals <- selectList [TestValueJson @>. String "true"] []
          [] `matchKeys` vals

      describe "@>. null queries" $ do

        it "matches nulls" $ db $ do
          -- null @>. null == True
          vals <- selectList [TestValueJson @>. Null] []
          [nullK,arrFilledK] `matchKeys` vals

        it "does not match null with string of null" $ db $ do
          -- null @>. "null" == False
          vals <- selectList [TestValueJson @>. String "null"] []
          [] `matchKeys` vals


      describe "<@. queries" $ do

        it "matches subobject when queried with superobject" $ db $ do
          -- {}                         <@. {"test":null,"test1":"no","blabla":[]} == True
          -- {"test":null,"test1":"no"} <@. {"test":null,"test1":"no","blabla":[]} == True
          let queryObject = object ["test" .= Null, "test1" .= String "no", "blabla" .= emptyArr]
          vals <- selectList [TestValueJson <@. queryObject] []
          [objNullK,objTestK] `matchKeys` vals

        it "matches raw values and sublists when queried with superlist" $ db $ do
          -- []    <@. [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
          -- null  <@. [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
          -- false <@. [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True

          -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] <@.
          -- [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True

          let queryList =
                toJSON [ Null, Number 4, String "b", Object mempty, emptyArr
                       , object [ "test" .= [Null], "test2" .= String "yes"]
                       , Bool False ]

          vals <- selectList [TestValueJson <@. queryList ] []
          [arrNullK,arrFilledK,boolFK,nullK] `matchKeys` vals

        it "matches identical strings" $ db $ do
          -- "a" <@. "a" == True
          vals <- selectList [TestValueJson <@. String "a"] []
          [strAK] `matchKeys` vals


        it "matches identical big floats" $ db $ do
          -- 9876543210.123457 <@ 9876543210.123457 == True
          vals <- selectList [TestValueJson <@. Number 9876543210.123457] []
          [numBigFloatK] `matchKeys` vals

        it "doesn't match different big floats" $ db $ do
          -- 9876543210.123457 <@. 9876543210.123456789 == False
          vals <- selectList [TestValueJson <@. Number 9876543210.123456789] []
          [] `matchKeys` vals

        it "matches nulls" $ db $ do
          -- null <@. null == True
          vals <- selectList [TestValueJson <@. Null] []
          [nullK] `matchKeys` vals

      describe "?. queries" $ do

        it "matches top level keys and not the keys of nested objects" $ db $ do
          void $ edgeCases

          -- {"test":null,"test1":"no"}                       ?. "test" == True
          -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?. "test" == False

          vals <- selectList [TestValueJson ?. "test"] []
          [objTestK] `matchKeys` vals

        it "doesn't match nested key" $ db $ do
          void $ edgeCases

          -- {"c":24.986,"foo":{"deep1":true"}} ?. "deep1" == False

          vals <- selectList [TestValueJson ?. "deep1"] []
          [] `matchKeys` vals

        it "matches \"{}\" but not empty object when queried with \"{}\"" $ db $ do
          void $ edgeCases

          -- "{}" ?. "{}" == True
          -- {}   ?. "{}" == False

          vals <- selectList [TestValueJson ?. "{}"] []
          [strObjK] `matchKeys` vals

        it "matches raw empty str and empty str key when queried with \"\"" $ db $ do
          EdgeCaseKeys {..} <- edgeCases

          ---- {}        ?. "" == False
          ---- ""        ?. "" == True
          ---- {"":9001} ?. "" == True

          vals <- selectList [TestValueJson ?. ""] []
          [strNullK,objEmptyK] `matchKeys` vals

        it "matches lists containing string value when queried with raw string value" $ db $ do
          EdgeCaseKeys {..} <- edgeCases

          -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?. "b" == True
          vals <- selectList [TestValueJson ?. "b"] []
          [arrFilledK,arrList4K,objFullK] `matchKeys` vals

        it "matches lists, objects, and raw values correctly when queried with string" $ db $ do
          EdgeCaseKeys {..} <- edgeCases

          -- [["a"]]                   ?. "a" == False
          -- "a"                       ?. "a" == True
          -- ["a","b","c","d"]         ?. "a" == True
          -- {"a":1,"b":2,"c":3,"d":4} ?. "a" == True

          vals <- selectList [TestValueJson ?. "a"] []
          [strAK,arrList4K,objFullK] `matchKeys` vals

        it "matches string list but not real list when queried with \"[]\"" $ db $ do
          EdgeCaseKeys {..} <- edgeCases

          -- "[]" ?. "[]" == True
          -- []   ?. "[]" == False

          vals <- selectList [TestValueJson ?. "[]"] []
          [strArrK] `matchKeys` vals

        it "does not match null when queried with string null" $ db $ do
          void $ edgeCases

          -- null ?. "null" == False

          vals <- selectList [TestValueJson ?. "null"] []
          [] `matchKeys` vals

        it "does not match bool whe nqueried with string bool" $ db $ do
          void $ edgeCases

          -- true ?. "true" == False

          vals <- selectList [TestValueJson ?. "true"] []
          [] `matchKeys` vals


      describe "?|. queries" $ do

        it "matches raw vals, lists, objects, and nested objects" $ db $ do
          EdgeCaseKeys {..} <- edgeCases

          -- "a"                                              ?|. ["a","b","c"] == True
          -- [["a"],1]                                        ?|. ["a","b","c"] == False
          -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?|. ["a","b","c"] == True
          -- ["a","b","c","d"]                                ?|. ["a","b","c"] == True
          -- {"a":1,"b":2,"c":3,"d":4}                        ?|. ["a","b","c"] == True

          vals <- selectList [TestValueJson ?|. ["a","b","c"]] []
          [strAK,arrFilledK,objDeepK,arrList4K,objFullK] `matchKeys` vals

        it "matches str object but not object when queried with \"{}\"" $ db $ do
          void $ edgeCases

          -- "{}"  ?|. ["{}"] == True
          -- {}    ?|. ["{}"] == False

          vals <- selectList [TestValueJson ?|. ["{}"]] []
          [strObjK] `matchKeys` vals

        it "doesn't match superstrings when queried with substring" $ db $ do
          void $ edgeCases

          -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?|. ["test"] == False
          -- "testing"                                        ?|. ["test"] == False
          -- {"test":null,"test1":"no"}                       ?|. ["test"] == True
          --
          vals <- selectList [TestValueJson ?|. ["test"]] []
          [objTestK] `matchKeys` vals

        it "doesn't match nested keys" $ db $ do
          void $ edgeCases
          -- {"c":24.986,"foo":{"deep1":true"}} ?|. ["deep1"] == False

          vals <- selectList [TestValueJson ?|. ["deep1"]] []
          [] `matchKeys` vals

        it "doesn't match anything when queried with empty list" $ db $ do
          void $ edgeCases

          -- ANYTHING ?|. [] == False
          vals <- selectList [TestValueJson ?|. []] []
          [] `matchKeys` vals

        it "doesn't match raw, non-string, values when queried with strings" $ db $ do
          void $ edgeCases

          -- true ?|. ["true","null","1"] == False
          -- null ?|. ["true","null","1"] == False
          -- 1    ?|. ["true","null","1"] == False

          vals <- selectList [TestValueJson ?|. ["true","null","1"]] []
          [] `matchKeys` vals

        it "matches string array when queried with \"[]\"" $ db $ do
          void $ edgeCases

          -- []   ?|. ["[]"] == False
          -- "[]" ?|. ["[]"] == True

          vals <- selectList [TestValueJson ?|. ["[]"]] []
          [strArrK] `matchKeys` vals

      describe "?&. queries" $ do

        it "matches anything when queried with an empty list" $ db $ do
          EdgeCaseKeys {..} <- edgeCases

          -- ANYTHING ?&. [] == True
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

        it "matches raw values, lists, and objects when queried with string" $ db $ do
          EdgeCaseKeys {..} <- edgeCases

          -- "a"                       ?&. ["a"] == True
          -- [["a"],1]                 ?&. ["a"] == False
          -- ["a","b","c","d"]         ?&. ["a"] == True
          -- {"a":1,"b":2,"c":3,"d":4} ?&. ["a"] == True

          vals <- selectList [TestValueJson ?&. ["a"]] []
          [strAK,arrList4K,objFullK] `matchKeys` vals

        it "matches raw values, lists, and objects when queried with multiple string" $ db $ do
          EdgeCaseKeys {..} <- edgeCases

          -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?&. ["b","c"] == False
          -- {"c":24.986,"foo":{"deep1":true"}}               ?&. ["b","c"] == False
          -- ["a","b","c","d"]                                ?&. ["b","c"] == True
          -- {"a":1,"b":2,"c":3,"d":4}                        ?&. ["b","c"] == True

          vals <- selectList [TestValueJson ?&. ["b","c"]] []
          [arrList4K,objFullK] `matchKeys` vals

        it "matches object string when queried with \"{}\"" $ db $ do
          void $ edgeCases

          -- {}   ?&. ["{}"] == False
          -- "{}" ?&. ["{}"] == True

          vals <- selectList [TestValueJson ?&. ["{}"]] []
          [strObjK] `matchKeys` vals

        it "doesn't match superstrings when queried with substring" $ db $ do
          void $ edgeCases

          -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?&. ["test"] == False
          -- "testing"                                        ?&. ["test"] == False
          -- {"test":null,"test1":"no"}                       ?&. ["test"] == True

          vals <- selectList [TestValueJson ?&. ["test"]] []
          [objTestK] `matchKeys` vals

        it "doesn't match nested keys" $ db $ do
          void $ edgeCases

          -- {"c":24.986,"foo":{"deep1":true"}} ?&. ["deep1"] == False
          vals <- selectList [TestValueJson ?&. ["deep1"]] []
          [] `matchKeys` vals

        it "doesn't match anything when there is a partial match" $ db $ do
          void $ edgeCases

          -- "a"                       ?&. ["a","e"] == False
          -- ["a","b","c","d"]         ?&. ["a","e"] == False
          -- {"a":1,"b":2,"c":3,"d":4} ?&. ["a","e"] == False
          vals <- selectList [TestValueJson ?&. ["a","e"]] []
          [] `matchKeys` vals

        it "matches string array when queried with \"[]\"" $ db $ do
          void $ edgeCases

          -- []   ?&. ["[]"] == False
          -- "[]" ?&. ["[]"] == True
          vals <- selectList [TestValueJson ?&. ["[]"]] []
          [strArrK] `matchKeys` vals

        it "doesn't match null when queried with string null" $ db $ do
          void $ edgeCases

          -- THIS WILL FAIL IF THE IMPLEMENTATION USES
          -- @ '{null}' @
          -- INSTEAD OF
          -- @ ARRAY['null'] @
          -- null ?&. ["null"] == False
          vals <- selectList [TestValueJson ?&. ["null"]] []
          [] `matchKeys` vals

        it "doesn't match number when queried with str of that number" $ db $ do
          void $ edgeCases
          str1 <- insert' $ toJSON $ String "1"

          -- [["a"],1] ?&. ["1"] == False
          -- "1"       ?&. ["1"] == True

          vals <- selectList [TestValueJson ?&. ["1"]] []
          [str1] `matchKeys` vals

        it "doesn't match empty objs or list when queried with empty string" $ db $ do
          EdgeCaseKeys {..} <- edgeCases

          -- {}        ?&. [""] == False
          -- []        ?&. [""] == False
          -- ""        ?&. [""] == True
          -- {"":9001} ?&. [""] == True

          vals <- selectList [TestValueJson ?&. [""]] []
          [strNullK,objEmptyK] `matchKeys` vals
