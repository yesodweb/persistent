{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME

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

(=@=) :: MonadIO m => String -> Bool -> m ()
s =@= b = liftIO $ assertBool s b

matchKeys :: (Show record, Show (Key record), MonadIO m, Eq (Key record))
          => String -> [Key record] -> [Entity record] -> m ()
matchKeys s ys xs = do
    msg1 =@= (xLen == yLen)
    forM_ ys $ \y -> msg2 y =@= (y `elem` ks)
  where ks = entityKey <$> xs
        xLen = length xs
        yLen = length ys
        msg1 = mconcat
            [ s, "\nexpected: ", show yLen
            , "\n but got: ", show xLen
            , "\n[xs: ", show xs
            , ", ys: ", show ys, "]"
            ]
        msg2 y = mconcat
            [ s, ": "
            , "key \"", show y
            , "\" not in result:\n  ", show ks
            ]

specs :: Spec
specs = describe "postgresql's JSON operators behave" $ do

  it "migrate, clean table, insert values and check queries" $ asIO $ runConn $ do
      runMigration jsonTestMigrate
      cleanDB

      liftIO $ putStrLn "\n- - - - -  Inserting JSON values  - - - - -\n"

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
      arrListK <- insert' $ toJSON ([emptyArr,emptyArr,toJSON [emptyArr,emptyArr]])
      arrList2K <- insert' $ toJSON [emptyArr,toJSON [Number 3,Bool False],toJSON [emptyArr,toJSON [Object mempty]]]
      arrFilledK <- insert' $ toJSON [Null, Number 4, String "b", Object mempty, emptyArr, object [ "test" .= [Null], "test2" .= String "yes"]]

      objNullK <- insert' $ Object mempty
      objTestK <- insert' $ object ["test" .= Null, "test1" .= String "no"]
      objDeepK <- insert' $ object ["c" .= Number 24.986, "foo" .= object ["deep1" .= Bool True]]

----------------------------------------------------------------------------------------

      liftIO $ putStrLn "\n- - - - -  Starting @> tests  - - - - -\n"

      -- An empty Object matches any object
      selectList [TestValueJson @>. Object mempty] []
        >>= matchKeys "1" [objNullK,objTestK,objDeepK]

      -- {"test":null,"test1":"no"} @> {"test":null} == True
      selectList [TestValueJson @>. object ["test" .= Null]] []
        >>= matchKeys "2" [objTestK]

      -- {"c":24.986,"foo":{"deep1":true"}} @> {"foo":{}} == True
      selectList [TestValueJson @>. object ["foo" .= object []]] []
        >>= matchKeys "3" [objDeepK]

      -- {"c":24.986,"foo":{"deep1":true"}} @> {"foo":"nope"} == False
      selectList [TestValueJson @>. object ["foo" .= String "nope"]] []
        >>= matchKeys "4" []

      -- {"c":24.986,"foo":{"deep1":true"}} @> {"foo":{"deep1":true}} == True
      selectList [TestValueJson @>. (object ["foo" .= object ["deep1" .= True]])] []
        >>= matchKeys "5" [objDeepK]

      -- {"c":24.986,"foo":{"deep1":true"}} @> {"deep1":true} == False
      selectList [TestValueJson @>. object ["deep1" .= True]] []
        >>= matchKeys "6" []

      -- An empty Array matches any array
      selectList [TestValueJson @>. emptyArr] []
        >>= matchKeys "7" [arrNullK,arrListK,arrList2K,arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [4] == True
      selectList [TestValueJson @>. toJSON [4 :: Int]] []
        >>= matchKeys "8" [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [null,"b"] == True
      selectList [TestValueJson @>. toJSON [Null, String "b"]] []
        >>= matchKeys "9" [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [null,"d"] == False
      selectList [TestValueJson @>. toJSON [emptyArr, String "d"]] []
        >>= matchKeys "10" []

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [[],"b",{"test":[null],"test2":"yes"},4,null,{}] == True
      selectList [TestValueJson @>. toJSON [emptyArr, String "b", object [ "test" .= [Null], "test2" .= String "yes"], Number 4, Null, Object mempty]] []
        >>= matchKeys "11" [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == False
      selectList [TestValueJson @>. toJSON [Null, Number 4, String "b", Object mempty, emptyArr, object [ "test" .= [Null], "test2" .= String "yes"], Bool False]] []
        >>= matchKeys "12" []

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [{}] == True
      selectList [TestValueJson @>. toJSON [Object mempty]] []
        >>= matchKeys "13" [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [{"test":[]}] == True
      selectList [TestValueJson @>. toJSON [object ["test" .= emptyArr]]] []
        >>= matchKeys "14" [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [{"test1":[null]}]  == False
      selectList [TestValueJson @>. toJSON [object ["test1" .= [Null]]]] []
        >>= matchKeys "15" []

      -- [[],[],[[],[]]]                                  @> [[]] == True
      -- [[],[3,false],[[],[{}]]]                         @> [[]] == True
      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> [[]] == True
      selectList [TestValueJson @>. toJSON [emptyArr]] []
        >>= matchKeys "16" [arrListK,arrList2K,arrFilledK]

      -- [[],[3,false],[[],[{}]]] @> [[3]] == True
      selectList [TestValueJson @>. toJSON [[3 :: Int]]] []
        >>= matchKeys "17" [arrList2K]

      -- [[],[3,false],[[],[{}]]] @> [[true,3]] == False
      selectList [TestValueJson @>. toJSON [[Bool True, Number 3]]] []
        >>= matchKeys "18" []

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> 4 == True
      selectList [TestValueJson @>. Number 4] []
        >>= matchKeys "19" [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> 4 == True
      selectList [TestValueJson @>. Number 99] []
        >>= matchKeys "20" []

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> "b" == True
      selectList [TestValueJson @>. String "b"] []
        >>= matchKeys "21" [arrFilledK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> "{}" == False
      selectList [TestValueJson @>. String "{}"] []
        >>= matchKeys "22" [strObjK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] @> {"test":[null],"test2":"yes"} == False
      selectList [TestValueJson @>. object [ "test" .= [Null], "test2" .= String "yes"]] []
        >>= matchKeys "23" []

      -- "testing" @> "testing" == True
      selectList [TestValueJson @>. String "testing"] []
        >>= matchKeys "24" [strTestK]

      -- "testing" @> "Testing" == False
      selectList [TestValueJson @>. String "Testing"] []
        >>= matchKeys "25" []

      -- "testing" @> "test" == False
      selectList [TestValueJson @>. String "test"] []
        >>= matchKeys "26" []

      -- "testing" @> {"testing":1} == False
      selectList [TestValueJson @>. object ["testing" .= Number 1]] []
        >>= matchKeys "27" []

      -- 1 @> 1 == True
      selectList [TestValueJson @>. toJSON (1 :: Int)] []
        >>= matchKeys "28" [num1K]

      -- 0 @> 0.0 == True
      -- 0.0 @> 0.0 == True
      selectList [TestValueJson @>. toJSON (0.0 :: Double)] []
        >>= matchKeys "29" [num0K,numFloatK]

      -- 1234567890 @> 123456789 == False
      selectList [TestValueJson @>. toJSON (123456789 :: Int)] []
        >>= matchKeys "30" []

      -- 1234567890 @> 234567890 == False
      selectList [TestValueJson @>. toJSON (234567890 :: Int)] []
        >>= matchKeys "31" []

      -- 1 @> "1" == False
      selectList [TestValueJson @>. String "1"] []
        >>= matchKeys "32" []

      -- 1234567890 @> [1,2,3,4,5,6,7,8,9,0] == False
      selectList [TestValueJson @>. toJSON ([1,2,3,4,5,6,7,8,9,0] :: [Int])] []
        >>= matchKeys "33" []

      -- true @> true == True
      -- false @> true == False
      selectList [TestValueJson @>. toJSON True] []
        >>= matchKeys "34" [boolTK]

      -- false @> false == True
      -- true @> false == False
      selectList [TestValueJson @>. Bool False] []
        >>= matchKeys "35" [boolFK]

      -- true @> "true" == False
      selectList [TestValueJson @>. String "true"] []
        >>= matchKeys "36" []

      -- null @> null == True
      selectList [TestValueJson @>. Null] []
        >>= matchKeys "37" [nullK,arrFilledK]

      -- null @> "null" == False
      selectList [TestValueJson @>. String "null"] []
        >>= matchKeys "38" []

----------------------------------------------------------------------------------------

      liftIO $ putStrLn "\n- - - - -  Starting <@ tests  - - - - -\n"

      -- {}                         <@ {"test":null,"test1":"no","blabla":[]} == True
      -- {"test":null,"test1":"no"} <@ {"test":null,"test1":"no","blabla":[]} == True
      selectList [TestValueJson <@. object ["test" .= Null, "test1" .= String "no", "blabla" .= emptyArr]] []
        >>= matchKeys "39" [objNullK,objTestK]

      -- []                                               <@ [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
      -- null                                             <@ [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
      -- false                                            <@ [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] <@ [null,4,"b",{},[],{"test":[null],"test2":"yes"},false] == True
      selectList [TestValueJson <@. toJSON [Null, Number 4, String "b", Object mempty, emptyArr, object [ "test" .= [Null], "test2" .= String "yes"], Bool False]] []
        >>= matchKeys "40" [arrNullK,arrFilledK,boolFK,nullK]

      -- "a" <@ "a" == True
      selectList [TestValueJson <@. String "a"] []
        >>= matchKeys "41" [strAK]


      -- 9876543210.123457 <@ 9876543210.123457 == False
      selectList [TestValueJson <@. Number 9876543210.123457] []
        >>= matchKeys "42" [numBigFloatK]

      -- 9876543210.123457 <@ 9876543210.123456789 == False
      selectList [TestValueJson <@. Number 9876543210.123456789] []
        >>= matchKeys "43" []

      -- null <@ null == True
      selectList [TestValueJson <@. Null] []
        >>= matchKeys "44" [nullK]

----------------------------------------------------------------------------------------

      liftIO $ putStrLn "\n- - - - -  Starting ? tests  - - - - -\n"

      arrList3K <- insert' $ toJSON [toJSON [String "a"], Number 1]
      arrList4K <- insert' $ toJSON [String "a", String "b", String "c", String "d"]
      objEmptyK <- insert' $ object ["" .= Number 9001]
      objFullK  <- insert' $ object ["a" .= Number 1, "b" .= Number 2, "c" .= Number 3, "d" .= Number 4]

      -- {"test":null,"test1":"no"}                       ? "test" == True
      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ? "test" == False
      selectList [TestValueJson ?. "test"] []
        >>= matchKeys "45" [objTestK]

      -- {"c":24.986,"foo":{"deep1":true"}} ? "deep1" == False
      selectList [TestValueJson ?. "deep1"] []
        >>= matchKeys "46" []

      -- "{}" ? "{}" == True
      -- {}   ? "{}" == False
      selectList [TestValueJson ?. "{}"] []
        >>= matchKeys "47" [strObjK]

      -- {}        ? "" == False
      -- ""        ? "" == True
      -- {"":9001} ? "" == True
      selectList [TestValueJson ?. ""] []
        >>= matchKeys "48" [strNullK,objEmptyK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ? "b" == True
      selectList [TestValueJson ?. "b"] []
        >>= matchKeys "49" [arrFilledK,arrList4K,objFullK]

      -- [["a"]]                   ? "a" == False
      -- "a"                       ? "a" == True
      -- ["a","b","c","d"]         ? "a" == True
      -- {"a":1,"b":2,"c":3,"d":4} ? "a" == True
      selectList [TestValueJson ?. "a"] []
        >>= matchKeys "50" [strAK,arrList4K,objFullK]

      -- "[]" ? "[]" == True
      -- []   ? "[]" == False
      selectList [TestValueJson ?. "[]"] []
        >>= matchKeys "51" [strArrK]

      -- null ? "null" == False
      selectList [TestValueJson ?. "null"] []
        >>= matchKeys "52" []

      -- true ? "true" == False
      selectList [TestValueJson ?. "true"] []
        >>= matchKeys "53" []

----------------------------------------------------------------------------------------

      liftIO $ putStrLn "\n- - - - -  Starting ?| tests  - - - - -\n"

      -- "a"                                              ?| ["a","b","c"] == True
      -- [["a"],1]                                        ?| ["a","b","c"] == False
      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?| ["a","b","c"] == True
      -- ["a","b","c","d"]                                ?| ["a","b","c"] == True
      -- {"a":1,"b":2,"c":3,"d":4}                        ?| ["a","b","c"] == True
      selectList [TestValueJson ?|. ["a","b","c"]] []
        >>= matchKeys "54" [strAK,arrFilledK,objDeepK,arrList4K,objFullK]

      -- "{}"  ?| ["{}"] == True
      -- {}    ?| ["{}"] == False
      selectList [TestValueJson ?|. ["{}"]] []
        >>= matchKeys "55" [strObjK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?| ["test"] == False
      -- "testing"                                        ?| ["test"] == False
      -- {"test":null,"test1":"no"}                       ?| ["test"] == True
      selectList [TestValueJson ?|. ["test"]] []
        >>= matchKeys "56" [objTestK]

      -- {"c":24.986,"foo":{"deep1":true"}} ?| ["deep1"] == False
      selectList [TestValueJson ?|. ["deep1"]] []
        >>= matchKeys "57" []

      -- ANYTHING ?| [] == False
      selectList [TestValueJson ?|. []] []
        >>= matchKeys "58" []

      -- true ?| ["true","null","1"] == False
      -- null ?| ["true","null","1"] == False
      -- 1    ?| ["true","null","1"] == False
      selectList [TestValueJson ?|. ["true","null","1"]] []
        >>= matchKeys "59" []

      -- []   ?| ["[]"] == False
      -- "[]" ?| ["[]"] == True
      selectList [TestValueJson ?|. ["[]"]] []
        >>= matchKeys "60" [strArrK]

----------------------------------------------------------------------------------------

      liftIO $ putStrLn "\n- - - - -  Starting ?& tests  - - - - -\n"

      -- ANYTHING ?& [] == True
      selectList [TestValueJson ?&. []] []
        >>= matchKeys "61" [ nullK
                           , boolTK, boolFK
                           , num0K, num1K, numBigK, numFloatK, numSmallK, numFloat2K, numBigFloatK
                           , strNullK, strObjK, strArrK, strAK, strTestK, str2K, strFloatK
                           , arrNullK, arrListK, arrList2K, arrFilledK
                           , objNullK, objTestK, objDeepK

                           , arrList3K, arrList4K
                           , objEmptyK, objFullK
                           ]

      -- "a"                       ?& ["a"] == True
      -- [["a"],1]                 ?& ["a"] == False
      -- ["a","b","c","d"]         ?& ["a"] == True
      -- {"a":1,"b":2,"c":3,"d":4} ?& ["a"] == True
      selectList [TestValueJson ?&. ["a"]] []
        >>= matchKeys "62" [strAK,arrList4K,objFullK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?& ["b","c"] == False
      -- {"c":24.986,"foo":{"deep1":true"}}               ?& ["b","c"] == False
      -- ["a","b","c","d"]                                ?& ["b","c"] == True
      -- {"a":1,"b":2,"c":3,"d":4}                        ?& ["b","c"] == True
      selectList [TestValueJson ?&. ["b","c"]] []
        >>= matchKeys "63" [arrList4K,objFullK]

      -- {}   ?& ["{}"] == False
      -- "{}" ?& ["{}"] == True
      selectList [TestValueJson ?&. ["{}"]] []
        >>= matchKeys "64" [strObjK]

      -- [null,4,"b",{},[],{"test":[null],"test2":"yes"}] ?& ["test"] == False
      -- "testing"                                        ?& ["test"] == False
      -- {"test":null,"test1":"no"}                       ?& ["test"] == True
      selectList [TestValueJson ?&. ["test"]] []
        >>= matchKeys "65" [objTestK]

      -- {"c":24.986,"foo":{"deep1":true"}} ?& ["deep1"] == False
      selectList [TestValueJson ?&. ["deep1"]] []
        >>= matchKeys "66" []

      -- "a"                       ?& ["a","e"] == False
      -- ["a","b","c","d"]         ?& ["a","e"] == False
      -- {"a":1,"b":2,"c":3,"d":4} ?& ["a","e"] == False
      selectList [TestValueJson ?&. ["a","e"]] []
        >>= matchKeys "67" []

      -- []   ?& ["[]"] == False
      -- "[]" ?& ["[]"] == True
      selectList [TestValueJson ?&. ["[]"]] []
        >>= matchKeys "68" [strArrK]

      -- THIS WILL FAIL IF THE IMPLEMENTATION USES
      -- @ '{null}' @
      -- INSTEAD OF
      -- @ ARRAY['null'] @
      -- null ?& ["null"] == False
      selectList [TestValueJson ?&. ["null"]] []
        >>= matchKeys "69" []

      -- [["a"],1] ?& ["1"] == False
      -- "1"       ?& ["1"] == True
      selectList [TestValueJson ?&. ["1"]] []
        >>= matchKeys "70" []

      -- {}        ?& [""] == False
      -- []        ?& [""] == False
      -- ""        ?& [""] == True
      -- {"":9001} ?& [""] == True
      selectList [TestValueJson ?&. [""]] []
        >>= matchKeys "71" [strNullK,objEmptyK]
