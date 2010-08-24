{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck
-- import Test.Framework.TH
import Test.HUnit hiding (Test)

import Database.Persist.Sqlite
import Control.Monad.IO.Class

import Control.Monad.Trans.Reader
import Monad (unless)
{-import Data.ByteString.Lazy.UTF8 (toString)-}
{-import Data.List (intercalate)-}

infix 1 /=@, @/=

(/=@) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
expected /=@ actual = liftIO $ assertNotEqual "" expected actual

(@/=) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
actual @/= expected = liftIO $ assertNotEqual "" expected actual

assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual preface expected actual =
  unless (actual /= expected) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n to not equal: " ++ show actual

infix 1 @==, ==@
expected @== actual = liftIO $ expected @?= actual
expected ==@ actual = liftIO $ expected @=? actual

mkPersist [$persist|
Empty

Person
    name String update Eq Ne Desc
    age Int update "Asc" Lt "some ignored attribute"
    color String null Eq Ne
    PersonNameKey name
Pet
    owner PersonId
    name String
|]

connstr :: String
connstr = "user=test password=test host=localhost port=5432 dbname=yesod_test"

runConn = (withSqlitePool connstr 1) . runSqlPool

sqliteTest :: SqlPersist IO () -> Assertion
sqliteTest actions = do runConn actions

setup :: SqlPersist IO ()
setup = do
  runMigration $ do
    migrate (undefined :: Person)
    migrate (undefined :: Pet)
  deleteWhere ([] :: [Filter Person])
  deleteWhere ([] :: [Filter Pet])

main :: IO ()
main = do
  runConn setup
  defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Database.Persistent"
    [
      testCase "sqlite persistent" case_sqlitePersistent
    , testCase "sqlite deleteWhere" case_sqliteDeleteWhere
    ]

                          
assertEmpty xs    = liftIO $ assertBool "" (null xs)
assertNotEmpty xs = liftIO $ assertBool "" (not (null xs))

case_sqliteDeleteWhere = sqliteTest _deleteWhere
case_sqlitePersistent = sqliteTest _persistent

_deleteWhere = do
  _ <- insert $ Person "Michael2" 27 Nothing
  pm2 <- selectList [PersonNameEq "Michael2"] [] 0 0
  assertNotEmpty pm2
  deleteWhere [PersonNameEq "Michael2"]
  pm2 <- selectList [PersonNameEq "Michael2"] [] 0 0
  assertEmpty pm2


_persistent = do
  let mic = Person "Michael" 25 Nothing
  micK <- insert mic
  Just p <- get micK
  p @== mic

  replace micK $ Person "Michael" 25 Nothing
  Just p <- get micK
  p @== mic

  replace micK $ Person "Michael" 26 Nothing
  Just mic26 <- get micK
  mic26 @/= mic
  personAge mic26 @== (personAge mic) + 1

  results <- selectList [PersonNameEq "Michael"] [] 0 0
  results @== [(micK, mic26)]

  results <- selectList [PersonAgeLt 28] [] 0 0
  results @== [(micK, mic26)]

  update micK [PersonAge 28]
  Just p28 <- get micK
  personAge p28 @== 28

  updateWhere [PersonNameEq "Michael"] [PersonAge 29]
  Just mic29 <- get micK
  personAge mic29 @== 29

  let eli = Person "Eliezer" 2 $ Just "blue"
  _ <- insert eli
  pasc <- selectList [] [PersonAgeAsc] 0 0
  (map snd pasc) @== [eli, mic29]

  let abe30 = Person "Abe" 30 $ Just "black"
  _ <- insert abe30 
  pdesc <- selectList [PersonAgeLt 30] [PersonNameDesc] 0 0
  (map snd pasc) @== [eli, mic29]

  let abe31 = Person "Abe" 31 $ Just "brown"
  -- error!
  _ <- insert abe31 
  abes <- selectList [PersonNameEq "Abe"] [] 0 0
  (map snd abes) @== [abe30]

  Just (k,p) <- getBy $ PersonNameKey "Michael"
  p @== mic29

  ps <- selectList [PersonColorEq $ Just "blue"] [] 0 0
  map snd ps @== [eli]

  ps <- selectList [PersonColorEq Nothing] [] 0 0
  map snd ps @== [mic29]

  ps <- selectList [PersonColorNe Nothing] [] 0 0
  map snd ps @== [eli, abe30]

  delete micK
  Nothing <- get micK
  return ()
