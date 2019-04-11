{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module JSONTest where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import qualified Data.Vector as V (fromList)
import Test.Hspec.Expectations ()

import Database.Persist
import Database.Persist.Postgresql.JSON

import PgInit

share [mkPersist persistSettings,  mkMigrate "jsonTestMigrate"] [persistLowerCase|
  TestValue
    json Value
|]

cleanDB :: (BaseBackend backend ~ SqlBackend, PersistQueryWrite backend, MonadIO m) => ReaderT backend m ()
cleanDB = deleteWhere ([] :: [Filter TestValue])

emptyArr :: Value
emptyArr = toJSON ([] :: [Value])

specs :: Spec
specs = describe "postgresql's @> and <@ operators behave" $ do


  let insert' = insert . TestValue
      lengthIs = (@==) . length
      matchKey x y = entityKey <$> x @== y

  it "migrate, clean table, insert values and check queries" $ asIO $ runConn $ do
      runMigration jsonTestMigrate
      cleanDB
      nullK <- insert' Null

      boolTK <- insert' $ Bool True
      boolFK <- insert' $ toJSON False

      num0K <- insert' $ Number 0
      num1K <- insert' $ Number 1
      _numBigK <- insert' $ toJSON (1234567890 :: Int)
      numFloatK <- insert' $ Number 0.0
      _numSmallK <- insert' $ Number 0.0000000000000000123
      _numFloat2K <- insert' $ Number 1.5
      -- numBigFloatK will turn into 9876543210.123457 because JSON
      numBigFloatK <- insert' $ toJSON (9876543210.123456789 :: Double)

      _strNullK <- insert' $ String ""
      strObjK <- insert' $ String "{}"
      _strArrK <- insert' $ String "[]"
      strAK <- insert' $ String "a"
      strTestK <- insert' $ toJSON ("testing" :: Text)
      _str2K <- insert' $ String "2"
      _strFloatK <- insert' $ String "0.45876"

      arrNullK <- insert' $ Array $ V.fromList []
      arrListK <- insert' $ toJSON ([emptyArr,emptyArr,toJSON [emptyArr,emptyArr]])
      arrList2K <- insert' $ toJSON [emptyArr,toJSON [Number 3,Bool False],toJSON [emptyArr,toJSON [Object mempty]]]
      arrFilledK <- insert' $ toJSON [Null, Number 4, String "b", Object mempty, emptyArr, object [ "test" .= [Null], "test2" .= String "yes"]]

      objNullK <- insert' $ Object mempty
      objTestK <- insert' $ object ["test" .= Null, "test1" .= String "no"]
      objDeepK <- insert' $ object ["c" .= Number 24.986, "foo" .= object ["deep1" .= Bool True]]

      -- An empty Object matches any object
      allObjs <- selectList [TestValueJson @>. Object mempty] []
      allObjs `lengthIs` 3
      let objKeys = entityKey <$> allObjs
      objNullK `elem` objKeys @== True
      objTestK `elem` objKeys @== True
      objDeepK `elem` objKeys @== True

      -- {"test":null,"test1":"no"} @> {"test":null} == True
      topLevelVal <- selectList [TestValueJson @>. object ["test" .= Null]] []
      topLevelVal `lengthIs` 1
      topLevelVal `matchKey` [objTestK]

      -- {"c":24.986,"foo":{"deep1":true"}} @> {"foo":{}} == True
      oneDeep <- selectList [TestValueJson @>. object ["foo" .= object []]] []
      oneDeep `lengthIs` 1
      oneDeep `matchKey` [objDeepK]

      -- {"c":24.986,"foo":{"deep1":true"}} @> {"foo":"nope"} == False
      wrongObjVal <- selectList [TestValueJson @>. object ["foo" .= String "nope"]] []
      wrongObjVal `lengthIs` 0

      -- {"c":24.986,"foo":{"deep1":true"}} @> {"foo":{"deep1":true}} == True
      twoDeep <- selectList [TestValueJson @>. (object ["foo" .= object ["deep1" .= True]])] []
      twoDeep `lengthIs` 1
      twoDeep `matchKey` [objDeepK]

      -- {"c":24.986,"foo":{"deep1":true"}} @> {"deep1":true} == False
      wrongDepth <- selectList [TestValueJson @>. object ["deep1" .= True]] []
      wrongDepth `lengthIs` 0

      -- An empty Array matches any array
      allArrs <- selectList [TestValueJson @>. emptyArr] []
      allArrs `lengthIs` 4
      let arrKeys = entityKey <$> allArrs
      arrNullK `elem` arrKeys @== True
      arrListK `elem` arrKeys @== True
      arrFilledK `elem` arrKeys @== True

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [4] == True
      valInArr <- selectList [TestValueJson @>. toJSON [4 :: Int]] []
      valInArr `lengthIs` 1
      valInArr `matchKey` [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [null,"b"] == True
      moreValsArr <- selectList [TestValueJson @>. toJSON [Null, String "b"]] []
      moreValsArr `lengthIs` 1
      moreValsArr `matchKey` [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [null,"d"] == False
      oneWrongVal <- selectList [TestValueJson @>. toJSON [emptyArr, String "d"]] []
      oneWrongVal `lengthIs` 0

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [[],"b",{"test":[null],"test2":"yes"},4,null,{}] == True
      orderNoMatter <- selectList [TestValueJson @>. toJSON [emptyArr, String "b", object [ "test" .= [Null], "test2" .= String "yes"], Number 4, Null, Object mempty]] []
      orderNoMatter `lengthIs` 1
      orderNoMatter `matchKey` [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == False
      moreThanExactly <- selectList [TestValueJson @>. toJSON [Null, Number 4, String "b", Object mempty, emptyArr, object [ "test" .= [Null], "test2" .= String "yes"], Bool False]] []
      moreThanExactly `lengthIs` 0

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [{}] == True
      emptyObjInArr <- selectList [TestValueJson @>. toJSON [Object mempty]] []
      emptyObjInArr `lengthIs` 1
      emptyObjInArr `matchKey` [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [{"test":[]}] == True
      objInArr <- selectList [TestValueJson @>. toJSON [object ["test" .= emptyArr]]] []
      objInArr `lengthIs` 1
      objInArr `matchKey` [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [{"test1":[null]}]  == False
      objNotInArr <- selectList [TestValueJson @>. toJSON [object ["test1" .= [Null]]]] []
      objNotInArr `lengthIs` 0

      -- [[],[],[[],[]]]                                  @> [[]] == True
      -- [[],[3,false],[[],[{}]]]                         @> [[]] == True
      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [[]] == True
      arrInArr <- selectList [TestValueJson @>. toJSON [emptyArr]] []
      arrInArr `lengthIs` 3
      let arrKeys2 = entityKey <$> arrInArr
      arrListK `elem` arrKeys2 @== True
      arrFilledK `elem` arrKeys2 @== True

      -- [[],[3,false],[[],[{}]]] @> [[3]] == True
      partialArrInArr <- selectList [TestValueJson @>. toJSON [[3 :: Int]]] []
      partialArrInArr `lengthIs` 1
      partialArrInArr `matchKey` [arrList2K]

      -- [[],[3,false],[[],[{}]]] @> [[true,3]] == False
      nestedWrongArr <- selectList [TestValueJson @>. toJSON [[Bool True, Number 3]]] []
      nestedWrongArr `lengthIs` 0

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> 4 == True
      singleMemberNum <- selectList [TestValueJson @>. Number 4] []
      singleMemberNum `lengthIs` 1
      singleMemberNum `matchKey` [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> 4 == True
      singleMemberWrongNum <- selectList [TestValueJson @>. Number 99] []
      singleMemberWrongNum `lengthIs` 0

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> "b" == True
      singleMemberStr <- selectList [TestValueJson @>. String "b"] []
      singleMemberStr `lengthIs` 1
      singleMemberStr `matchKey` [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> "{}" == False
      singleMemberWrongStr <- selectList [TestValueJson @>. String "{}"] []
      singleMemberWrongStr `lengthIs` 1
      singleMemberWrongStr `matchKey` [strObjK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> {"test":[null],"test2":"yes"} == False
      singleMemberObj <- selectList [TestValueJson @>. object [ "test" .= [Null], "test2" .= String "yes"]] []
      singleMemberObj `lengthIs` 0

      -- "testing" @> "testing" == True
      exactMatchStr <- selectList [TestValueJson @>. String "testing"] []
      exactMatchStr `lengthIs` 1
      exactMatchStr `matchKey` [strTestK]

      -- "testing" @> "Testing" == False
      justOffStr <- selectList [TestValueJson @>. String "Testing"] []
      justOffStr `lengthIs` 0

      -- "testing" @> "test" == False
      partialStr <- selectList [TestValueJson @>. String "test"] []
      partialStr `lengthIs` 0

      -- "testing" @> {"testing":1} == False
      keyOfObjStr <- selectList [TestValueJson @>. object ["testing" .= Number 1]] []
      keyOfObjStr `lengthIs` 0

      -- 1 @> 1 == True
      exactMatchNum <- selectList [TestValueJson @>. toJSON (1 :: Int)] []
      exactMatchNum `lengthIs` 1
      exactMatchNum `matchKey` [num1K]

      -- 0 @> 0.0 == True
      -- 0.0 @> 0.0 == True
      exactMatchFloat <- selectList [TestValueJson @>. toJSON (0.0 :: Double)] []
      exactMatchFloat `lengthIs` 2
      let floatsKeys = entityKey <$> exactMatchFloat
      num0K `elem` floatsKeys @== True
      numFloatK `elem` floatsKeys @== True

      -- 1234567890 @> 123456789 == False
      partialNum1 <- selectList [TestValueJson @>. toJSON (123456789 :: Int)] []
      partialNum1 `lengthIs` 0

      -- 1234567890 @> 234567890 == False
      partialNum2 <- selectList [TestValueJson @>. toJSON (234567890 :: Int)] []
      partialNum2 `lengthIs` 0

      -- 1 @> "1" == False
      strMatchNum <- selectList [TestValueJson @>. String "1"] []
      strMatchNum `lengthIs` 0

      -- 1234567890 @> [1,2,3,4,5,6,7,8,9,0] == False
      numsInArr <- selectList [TestValueJson @>. toJSON ([1,2,3,4,5,6,7,8,9,0] :: [Int])] []
      numsInArr `lengthIs` 0

      -- true @> true == True
      -- false @> true == False
      boolMatch <- selectList [TestValueJson @>. toJSON True] []
      boolMatch `lengthIs` 1
      boolMatch `matchKey` [boolTK]

      -- false @> false == True
      -- true @> false == False
      boolMatch2 <- selectList [TestValueJson @>. Bool False] []
      boolMatch2 `lengthIs` 1
      boolMatch2 `matchKey` [boolFK]

      -- true @> "true" == False
      boolStrMatch <- selectList [TestValueJson @>. String "true"] []
      boolStrMatch `lengthIs` 0

      -- null @> null == True
      nullMatch <- selectList [TestValueJson @>. Null] []
      nullMatch `lengthIs` 2
      let nullKeys = entityKey <$> nullMatch
      nullK `elem` nullKeys @== True
      arrFilledK `elem` nullKeys @== True

      -- null @> "null" == False
      nullStrMatch <- selectList [TestValueJson @>. String "null"] []
      nullStrMatch `lengthIs` 0

      -- {}                         <@ {"test":null,"test1":"no","blabla":[]} == True
      -- {"test":null,"test1":"no"} <@ {"test":null,"test1":"no","blabla":[]} == True
      biggerObjMatch <- selectList [TestValueJson <@. object ["test" .= Null, "test1" .= String "no", "blabla" .= emptyArr]] []
      biggerObjMatch `lengthIs` 2
      let biggerObjKeys = entityKey <$> biggerObjMatch
      objNullK `elem` biggerObjKeys @== True
      objTestK `elem` biggerObjKeys @== True

      -- []                                               <@ [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
      -- null                                             <@ [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
      -- false                                            <@ [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] <@ [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
      biggerArrMatch <- selectList [TestValueJson <@. toJSON [Null, Number 4, String "b", Object mempty, emptyArr, object [ "test" .= [Null], "test2" .= String "yes"], Bool False]] []
      biggerArrMatch `lengthIs` 4
      let biggerArrKeys = entityKey <$> biggerArrMatch
      arrNullK `elem` biggerArrKeys @== True
      arrFilledK `elem` biggerArrKeys @== True
      boolFK `elem` biggerArrKeys @== True
      nullK `elem` biggerArrKeys @== True

      -- "a" <@ "a" == True
      strMatch <- selectList [TestValueJson <@. String "a"] []
      strMatch `lengthIs` 1
      strMatch `matchKey` [strAK]


      -- 9876543210.123457 <@ 9876543210.123457 == False
      numMatch <- selectList [TestValueJson <@. Number 9876543210.123457] []
      numMatch `lengthIs` 1
      numMatch `matchKey` [numBigFloatK]

      -- 9876543210.123457 <@ 9876543210.123456789 == False
      numMisMatch <- selectList [TestValueJson <@. Number 9876543210.123456789] []
      numMisMatch `lengthIs` 0

      -- null <@ null == True
      nullMatch2 <- selectList [TestValueJson <@. Null] []
      nullMatch2 `lengthIs` 1
      nullMatch2 `matchKey` [nullK]
