{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistUniqueTest where

import Init

-- mpsGeneric = False is due to a bug or at least lack of a feature in mkKeyTypeDec TH.hs
share
    [mkPersist persistSettings{mpsGeneric = False}, mkMigrate "migration"]
    [persistLowerCase|

Fo
    foo Int
    bar Int
    Primary foo
    UniqueBar bar
    deriving Eq Show

Ba
    foo Int
    baz Int
    UniqueBaz baz
    deriving Eq Show

OnlyPrimaryKey
    foo Int
    name String
    Primary foo
    deriving Eq Show

|]

deriving stock instance Eq (Unique Fo)
deriving stock instance Show (Unique Fo)

deriving stock instance Show (Unique Ba)
deriving stock instance Eq (Unique Ba)

shouldCompile
    :: (OnlyOneUniqueKey OnlyPrimaryKey, AtLeastOneUniqueKey OnlyPrimaryKey) => IO ()
shouldCompile = pure ()

cleanDB
    :: (MonadIO m, PersistQuery backend, PersistEntityBackend Fo ~ backend)
    => ReaderT backend m ()
cleanDB = do
    deleteWhere ([] :: [Filter Fo])

specsWith :: (Runner SqlBackend m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "PersistUniqueTest" $ do
    describe "getBy" $ do
        it "works to pull a record from the database" $ runDb $ do
            let
                b = 5
            k <- insert Fo{foFoo = 3, foBar = b}
            Just vk <- get k
            Just vu <- getBy (UniqueBar b)
            vu @== Entity k vk

    describe "existsBy" $ do
        it "works to query the existence of a record in the database" $ runDb $ do
            let
                b = 5
            k <- insert Fo{foFoo = 3, foBar = b}
            result <- existsBy $ UniqueBar b
            result @== True

        it "returns false for nonexistent records" $ runDb $ do
            insert_ Fo{foFoo = 3, foBar = 5}
            result <- existsBy $ UniqueBar 17
            result @== False

    describe "insertUniqueEntity" $ do
        it "inserts a value if no conflicts are present" $ runDb $ do
            let
                fo = Fo 3 5
            Just (Entity _ insertedFoValue) <- insertUniqueEntity fo
            fo @== insertedFoValue

        it "does not insert if the record is entirely the same" $ runDb $ do
            let
                fo = Fo 3 5
            Just (Entity _ insertedFoValue) <- insertUniqueEntity fo
            mresult <- insertUniqueEntity fo
            mresult @== Nothing

        it "does not insert if there is a primary key conflict" $ runDb $ do
            let
                fo = Fo 3 5
            Just (Entity _ insertedFoValue) <- insertUniqueEntity fo
            mresult <- insertUniqueEntity fo{foFoo = 4}
            mresult @== Nothing

        it "does not insert if there is a unique key conflict" $ runDb $ do
            let
                fo = Fo 3 5
            Just (Entity _ insertedFoValue) <- insertUniqueEntity fo
            mresult <- insertUniqueEntity fo{foBar = 4}
            mresult @== Nothing

    describe "checkUniqueUpdateable" $ do
        describe "with standard id" $ do
            it "returns the unique constraint that failed" $ runDb $ do
                let
                    ba = Ba{baFoo = 1, baBaz = 2}
                bk <- insert ba
                mresult <- checkUnique ba
                mresult @== Just (UniqueBaz 2)
            it "returns Nothing if no constraint conflict exists" $ runDb $ do
                let
                    ba = Ba{baFoo = 1, baBaz = 2}
                mresult <- checkUnique ba
                mresult @== Nothing

        describe "with Primary" $ do
            it "conflicts with itself" $ runDb $ do
                let
                    f = 3
                let
                    b = 5
                let
                    fo = Fo f b
                k <- insert fo
                mresult <- checkUnique fo
                mresult @== Just (FoPrimaryKey f)

            it "returns the key that failed" $ runDb $ do
                let
                    f = 3
                let
                    b = 5
                let
                    fo = Fo f b
                k <- insert fo
                _ <- checkUnique fo -- conflicts with itself
                let
                    fo' = Fo (f + 1) b
                Just _ <- checkUnique fo' -- conflicts with fo
                Nothing <- checkUniqueUpdateable $ Entity k fo' -- but fo can be updated to fo'
                let
                    fo'' = Fo (f + 1) (b + 1)
                insert_ fo''
                mresult <- checkUniqueUpdateable $ Entity k fo'' -- fo can't be updated to fo''
                mresult @== Just (FoPrimaryKey (f + 1))

    describe "upsert" $ do
        describe "OnlyPrimaryKey" $ do
            it "can upsert" $ runDb $ do
                let
                    record =
                        OnlyPrimaryKey
                            { onlyPrimaryKeyFoo = 1
                            , onlyPrimaryKeyName = "Oh no"
                            }
                entity <- upsert record [OnlyPrimaryKeyName =. "Hello"]
                entityVal entity @== record
                entity' <- upsert record [OnlyPrimaryKeyName =. "Hello"]
                entityVal entity' @== record{onlyPrimaryKeyName = "Hello"}

        describe "Fo" $ do
            it "cannot upsert" $ runDb $ do
                -- uncomment to verify
                -- _ <- upsert Fo { foFoo = 1, foBar = 2 } [FoFoo +=. 1]
                pure ()
            it "can upsertBy" $ runDb $ do
                let
                    f = Fo{foFoo = 1, foBar = 2}
                entity <- upsertBy (FoPrimaryKey 1) f [FoBar +=. 1]
                entityVal entity @== f
                entity' <- upsertBy (FoPrimaryKey 1) f [FoBar +=. 1]
                entityVal entity' @== f{foBar = 1 + foBar f}

    describe "OnlyPrimaryKey" $ do
        it "has unique constraints" $ do
            shouldCompile
