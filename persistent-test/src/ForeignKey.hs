{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications, UndecidableInstances #-}

module ForeignKey where

import Data.Proxy
import qualified Data.List as List
import Init

-- mpsGeneric = False is due to a bug or at least lack of a feature in mkKeyTypeDec TH.hs
share [mkPersist persistSettings { mpsGeneric = False }, mkMigrate "compositeMigrate"] [persistLowerCase|
SimpleCascadeChild
    ref SimpleCascadeId OnDeleteCascade
    deriving Show Eq

SimpleCascade
    name String
    deriving Show Eq

Parent
    name String
    Primary name

Child
    pname String
    Foreign Parent OnDeleteCascade OnUpdateCascade fkparent pname
    deriving Show Eq

ParentComposite
    name String
    lastName String
    Primary name lastName

ChildComposite
    pname String
    plastName String
    Foreign ParentComposite OnDeleteCascade fkparent pname plastName
    deriving Show Eq

SelfReferenced
    name String
    pname String
    Primary name
    Foreign SelfReferenced OnDeleteCascade fkparent pname
    deriving Show Eq
|]

specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = fdescribe "foreign keys options" $ do
    it "delete cascades" $ runDb $ do
        kf <- insert $ Parent "A"
        kc <- insert $ Child "A"
        delete kf
        cs <- selectList [] []
        let expected = [] :: [Entity Child]
        cs @== expected
    it "update cascades" $ runDb $ do
        kf <- insert $ Parent "A"
        kc <- insert $ Child "A"
        update kf [ParentName =. "B"]
        cs <- selectList [] []
        fmap (childPname . entityVal) cs @== ["B"]
    it "delete Composite cascades" $ runDb $ do
        kf <- insert $ ParentComposite "A" "B"
        kc <- insert $ ChildComposite "A" "B"
        delete kf
        cs <- selectList [] []
        let expected = [] :: [Entity ChildComposite]
        cs @== expected
    it "delete self referenced cascades" $ runDb $ do
        kf <- insert $ SelfReferenced "A" "A" -- bootstrap self reference
        kc <- insert $ SelfReferenced "B" "A"
        delete kf
        srs <- selectList [] []
        let expected = [] :: [Entity SelfReferenced]
        srs @== expected
    it "delete cascade works on simple references" $ runDb $ do
        scId <- insert $ SimpleCascade "Hello"
        sccId <- insert $ SimpleCascadeChild scId
        Just _ <- get sccId
        delete scId
        mres <- get sccId
        mxs <- selectList @SimpleCascadeChild [] []
        liftIO $ do
            mres `shouldBe` Nothing
            mxs `shouldBe` []

    describe "EntityDef" $ do
        let ed =
                entityDef (Proxy @SimpleCascadeChild)
            isRefCol =
                (HaskellName "ref" ==) . fieldHaskell
            expected = FieldCascade
                { fcOnUpdate = Nothing
                , fcOnDelete = Just Cascade
                }
            Just refField =
                List.find isRefCol (entityFields ed)

        it "parses into fieldCascade"  $ do
            fieldCascade refField `shouldBe` expected

        it "parses into the fieldReference" $ do
            ForeignRef haskName _ cascade <-
                pure $ fieldReference refField

            haskName `shouldBe` HaskellName "SimpleCascade"

            cascade `shouldBe` expected

        it "shouldn't have cascade in extras" $ do
            entityExtra ed
                `shouldBe`
                    mempty
