{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module PersistUniqueTest where

import Init

-- mpsGeneric = False is due to a bug or at least lack of a feature in mkKeyTypeDec TH.hs
share [mkPersist persistSettings { mpsGeneric = False }, mkMigrate "migration"] [persistLowerCase|
  Fo
      foo Int
      bar Int
      Primary foo
      UniqueBar bar
      deriving Eq Show
|]

cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend Fo ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Fo])

specsWith :: Runner SqlBackend m => RunDb SqlBackend m -> Spec
specsWith runDb = describe "custom primary key" $ do
  it "getBy" $ runDb $ do
    let b = 5
    k <- insert $ Fo 3 b
    Just vk <- get k
    Just vu <- getBy (UniqueBar b)
    vu @== Entity k vk
  it "insertUniqueEntity" $ runDb $ do
    let fo = Fo 3 5
    Just (Entity _ insertedFoValue) <- insertUniqueEntity fo
    Nothing <- insertUniqueEntity fo
    fo @== insertedFoValue
