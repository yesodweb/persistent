{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module UniqueTest where

import Init
import Control.Monad (void)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Word
import Test.HUnit (Assertion)
import Test.Hspec (shouldThrow, anyException)
#ifndef WITH_MONGODB
import Database.Persist.GenericSql (Checkmark(..))
#endif

#ifdef WITH_MONGODB
mkPersist persistSettings [persist|
#else
share [mkPersist sqlSettings,  mkMigrate "uniqueMigrate"] [persist|
#endif
  TestNonNull
    fieldA Int
    UniqueTestNonNull fieldA
    deriving Eq Show
  TestNull
    fieldA Int
    fieldB Int Maybe
    UniqueTestNull fieldA fieldB !force
    deriving Eq Show
#ifndef WITH_MONGODB
  TestCheckmark
    key   String
    value String
    active Checkmark nullable
    UniqueTestCheckmark key active !force
    deriving Eq Show
#endif
|]
#ifdef WITH_MONGODB
cleanDB :: (PersistQuery m, PersistEntityBackend Number ~ PersistMonadBackend m) => m ()
cleanDB = do
  deleteWhere ([] :: [Filter TestNonNull])
  deleteWhere ([] :: [Filter TestNull])
db :: Action IO () -> Assertion
db = db' cleanDB
#endif

specs :: Spec
specs = describe "uniqueness constraints" $ do
  it "are respected for non-nullable Ints" $ do
    let ins = insert . TestNonNull
    (db $ void $ ins 1 >>        ins 2)
    (db $ void $ ins 1 >>        ins 2 >> ins 1) `shouldThrow` anyException
    (db $ void $ ins 1 >>= \k -> ins 2 >> delete k >> ins 1)
  it "are respected for nullable Ints" $ do
    let ins a b = insert $ TestNull a b
        ctx = ins 1 Nothing  >> ins 1 Nothing >> ins 1 Nothing >>
              ins 1 (Just 3) >> ins 1 (Just 4)
    (db $ void   ctx)
    (db $ void $ ctx >> ins 1 (Just 3)) `shouldThrow` anyException
    (db $ void $ ctx >> ins 1 (Just 4)) `shouldThrow` anyException
    (db $ void $ ctx >>= \k -> delete k >> ins 1 (Just 4))
#ifndef WITH_MONGODB
  it "work for Checkmark" $ do
    let ins k v a = insert $ TestCheckmark k v a
        ctx = ins "name" "John"    Inactive
           >> ins "name" "Stewart" Inactive
           >> ins "name" "Doroty"  Active
           >> ins "color" "blue"   Inactive
    (db $ void ctx)
    (db $ void $ ctx >> ins "name" "Melissa" Active) `shouldThrow` anyException
    (db $ void $ ctx >> ins "name" "Melissa" Inactive)
    (db $ void $ ctx >>= flip update [TestCheckmarkActive =. Active])
    (db $ void $ do
        void ctx
        updateWhere [TestCheckmarkKey   ==. "name"]
                    [TestCheckmarkActive =. Inactive]
        ins "name" "Melissa" Active)
#endif
