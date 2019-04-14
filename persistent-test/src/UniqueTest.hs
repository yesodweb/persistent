{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module UniqueTest where

import Init

share [mkPersist sqlSettings,  mkMigrate "uniqueMigrate"] [persistLowerCase|
  TestNonNull
    fieldA Int
    UniqueTestNonNull fieldA sql=UniqueTestNonNull !force
    deriving Eq Show
  TestNull
    fieldA Int
    fieldB Int Maybe
    UniqueTestNull fieldA fieldB sql=UniqueTestNonNullSqlName !force
    deriving Eq Show

  TestCheckmark
    name   Text
    value  Text
    active Checkmark nullable
    UniqueTestCheckmark name active !force
    deriving Eq Show
|]

cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend TestNonNull ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter TestNonNull])
  deleteWhere ([] :: [Filter TestNull])

specsWith :: Runner SqlBackend m => RunDb SqlBackend m -> Spec
specsWith runDb =
  describe "uniqueness constraints" $ do
    it "are respected for non-nullable Ints" $ do
      let ins = insert . TestNonNull
      (runDb $ void $ ins 1 >>        ins 2)
      (runDb $ void $ ins 1 >>        ins 2 >> ins 1) `shouldThrow` anyException
      (runDb $ void $ ins 1 >>= \k -> ins 2 >> delete k >> ins 1)
    it "are respected for nullable Ints" $ do
      let ins a b = insert $ TestNull a b
          ctx = ins 1 Nothing  >> ins 1 Nothing >> ins 1 Nothing >>
                ins 1 (Just 3) >> ins 1 (Just 4)
      (runDb $ void   ctx)
      (runDb $ void $ ctx >> ins 1 (Just 3)) `shouldThrow` anyException
      (runDb $ void $ ctx >> ins 1 (Just 4)) `shouldThrow` anyException
      (runDb $ void $ ctx >>= \k -> delete k >> ins 1 (Just 4))
    it "work for Checkmark" $ do
      let ins k v a = insert $ TestCheckmark k v a
          ctx = ins "name" "John"    Inactive
             >> ins "name" "Stewart" Inactive
             >> ins "name" "Doroty"  Active
             >> ins "color" "blue"   Inactive
      (runDb $ void ctx)
      (runDb $ void $ ctx >> ins "name" "Melissa" Active) `shouldThrow` anyException
      (runDb $ void $ ctx >> ins "name" "Melissa" Inactive)
      (runDb $ void $ ctx >>= flip update [TestCheckmarkActive =. Active])
      (runDb $ void $ do
          void ctx
          updateWhere [TestCheckmarkName   ==. "name"]
                      [TestCheckmarkActive =. Inactive]
          ins "name" "Melissa" Active)
