{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module DeleteCascadeTest where

import Init
import Database.Persist.TH (mkDeleteCascade)


share [mkPersist sqlSettings, mkMigrate "migrateAll", mkDeleteCascade sqlSettings] [persistLowerCase|
PersonX
    username String
    name String
    age Int Maybe
    UniqueName name
    Primary username
    deriving Show
BlogPostX
    title String
    authorId PersonXId
    deriving Show
AliasX
    person String
    alias String
    -- TODO: Uncomment when we can implement non-primary key foreign key
    -- references.
    -- Foreign Person fkperson person name
    deriving Show
|]

specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "DeleteCascade" $ do
    it "should delete by primary key reference" $ runDb $ do
        pk <-insert (PersonX "username0" "name0" Nothing)
        insertMany_ [BlogPostX "b0" pk, BlogPostX "b1" pk]
        i <- length <$> selectList [BlogPostXAuthorId ==. pk] []
        liftIO $ i `shouldBe` 2
        deleteCascade pk
        xs <- selectList [BlogPostXAuthorId ==. pk] []
        liftIO $ void xs `shouldBe` []

    it "should delete by non-primary key refernece" $ do
        pendingWith "Depends on specifying non-PK references: see issue #909"
  where
    setup = do
        runDb $ do
            runMigrationUnsafe migrateAll
            deleteWhere ([] :: [Filter PersonX])
            deleteWhere ([] :: [Filter BlogPostX])
            deleteWhere ([] :: [Filter AliasX])
