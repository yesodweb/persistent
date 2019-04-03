{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module RenameTestMongo where

import Data.Time (getCurrentTime, Day, UTCTime(..))
import qualified Data.Map as Map
import qualified Data.Text as T
import InitMongo

-- persistent used to not allow types with an "Id" suffix
-- this verifies that the issue is fixed
type TextId = Text

-- Test lower case names
mkPersist persistSettings [persistUpperCase|
-- This just tests that a field can be named "key"
KeyTable
    key Text
    deriving Eq Show

IdTable
    Id   Day default=CURRENT_DATE
    name Text
    -- This was added to test the ability to break a cycle
    -- getting rid of the Maybe should be a compilation failure
    keyTableEmbed IdTable Maybe
    deriving Eq Show

LowerCaseTable
    Id            sql=my_id
    fullName Text
    ExtraBlock
        foo bar
        baz
        bin
    ExtraBlock2
        something

RefTable
    someVal Int sql=something_else
    lct LowerCaseTableId
    text TextId
    UniqueRefTable someVal

-- Test a reference to a non-int Id
ForeignIdTable
    idId IdTableId
|]

cleanDB :: ReaderT Context IO ()
cleanDB = do
  deleteWhere ([] :: [Filter IdTable])
  deleteWhere ([] :: [Filter LowerCaseTable])
  deleteWhere ([] :: [Filter RefTable])

db :: Action IO () -> Assertion
db = db' cleanDB

specs :: Spec
specs = describe "rename specs" $ do
    it "user specified id, insertKey, no default=" $ db $ do
      let rec2 = IdTable "Foo2" Nothing
      let rec1 = IdTable "Foo1" $ Just rec2
      let rec  = IdTable "Foo" $ Just rec1
      now <- liftIO getCurrentTime
      let key = IdTableKey $ utctDay now
      insertKey key rec
      Just rec' <- get key
      rec' @== rec
      (Entity key' _):_ <- selectList ([] :: [Filter IdTable]) []
      key' @== key

    it "extra blocks" $
        entityExtra (entityDef (Nothing :: Maybe LowerCaseTable)) @?=
            Map.fromList
                [ ("ExtraBlock", map T.words ["foo bar", "baz", "bin"])
                , ("ExtraBlock2", map T.words ["something"])
                ]

asIO :: IO a -> IO a
asIO = id
