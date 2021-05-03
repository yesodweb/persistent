{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, UndecidableInstances #-}

module ForeignKey where

import Data.Proxy
import qualified Data.List as List
import Init

import Database.Persist.EntityDef.Internal (entityExtra)

-- mpsGeneric = False is due to a bug or at least lack of a feature in mkKeyTypeDec TH.hs
share [mkPersist persistSettings { mpsGeneric = False }, mkMigrate "compositeMigrate"] [persistLowerCase|
SimpleCascadeChild
    ref SimpleCascadeId OnDeleteCascade
    deriving Show Eq

SimpleCascade
    name Int
    deriving Show Eq

Parent
    name Int
    Primary name

Child
    pname Int
    Foreign Parent OnDeleteCascade OnUpdateCascade fkparent pname
    deriving Show Eq

ParentImplicit
    name Int

ChildImplicit
    pname Int
    parentId ParentImplicitId noreference
    Foreign ParentImplicit OnDeleteCascade OnUpdateCascade fkparent parentId
    deriving Show Eq

ParentComposite
    name Int
    lastName Int
    Primary name lastName

ChildComposite
    pname Int
    plastName Int
    Foreign ParentComposite OnDeleteCascade fkparent pname plastName
    deriving Show Eq

SelfReferenced
    name Int
    pname Int
    Primary name
    Foreign SelfReferenced OnDeleteCascade fkparent pname
    deriving Show Eq

A
    aa Int
    ab Int
    U1 aa

B
    ba Int
    bb Int
    Foreign A OnDeleteCascade fkA ba References aa
    deriving Show Eq

AComposite
    aa Int
    ab Int
    U2 aa ab

BComposite
    ba Int
    bb Int
    Foreign AComposite OnDeleteCascade fkAComposite ba bb References aa ab
    deriving Show Eq

BExplicit
    ba AId noreference
    Foreign A OnDeleteCascade fkAI ba References Id
    deriving Show Eq

Chain
    name Int
    previous ChainId Maybe noreference
    Foreign Chain OnDeleteSetNull fkChain previous References Id
    deriving Show Eq Ord

Chain2
    name Int
    previous Chain2Id Maybe noreference
    Foreign Chain2 OnDeleteCascade fkChain previous References Id
    deriving Show Eq

Chain3
    name Int
    previous Chain3Id Maybe OnDeleteCascade
    deriving Show Eq
|]

specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "foreign keys options" $ do
    it "delete cascades" $ runDb $ do
        kf <- insert $ Parent 1
        insert_ $ Child 1
        delete kf
        cs <- selectList [] []
        let expected = [] :: [Entity Child]
        cs @== expected
    it "update cascades" $ runDb $ do
        kf <- insert $ Parent 1
        insert_ $ Child 1
        update kf [ParentName =. 2]
        cs <- selectList [] []
        fmap (childPname . entityVal) cs @== [2]
    it "delete Composite cascades" $ runDb $ do
        kf <- insert $ ParentComposite 1 2
        insert_ $ ChildComposite 1 2
        delete kf
        cs <- selectList [] []
        let expected = [] :: [Entity ChildComposite]
        cs @== expected
    it "delete self referenced cascades" $ runDb $ do
        kf <- insert $ SelfReferenced 1 1
        insert_ $ SelfReferenced 2 1
        delete kf
        srs <- selectList [] []
        let expected = [] :: [Entity SelfReferenced]
        srs @== expected
    it "delete cascade works on simple references" $ runDb $ do
        scId <- insert $ SimpleCascade 1
        sccId <- insert $ SimpleCascadeChild scId
        Just _ <- get sccId
        delete scId
        mres <- get sccId
        mxs <- selectList @SimpleCascadeChild [] []
        liftIO $ do
            mres `shouldBe` Nothing
            mxs `shouldBe` []
    it "delete cascades with explicit Reference" $ runDb $ do
        kf <- insert $ A 1 40
        insert_ $ B 1 15
        delete kf
        return ()
        cs <- selectList [] []
        let expected = [] :: [Entity B]
        cs @== expected
    it "delete cascades with explicit Composite Reference" $ runDb $ do
        kf <- insert $ AComposite 1 20
        insert_ $ BComposite 1 20
        delete kf
        return ()
        cs <- selectList [] []
        let expected = [] :: [Entity B]
        cs @== expected
    it "delete cascades with explicit Composite Reference" $ runDb $ do
        kf <- insert $ AComposite 1 20
        insert_ $ BComposite 1 20
        delete kf
        return ()
        cs <- selectList [] []
        let expected = [] :: [Entity B]
        cs @== expected
    it "delete cascades with explicit Id field" $ runDb $ do
        kf <- insert $ A 1 20
        insert_ $ BExplicit kf
        delete kf
        return ()
        cs <- selectList [] []
        let expected = [] :: [Entity B]
        cs @== expected
    it "deletes sets null with self reference" $ runDb $ do
        kf <- insert $ Chain 1 Nothing
        kf' <- insert $ Chain 2 (Just kf)
        delete kf
        cs <- selectList [] []
        let expected = [Entity {entityKey = kf', entityVal = Chain 2 Nothing}]
        List.sort cs @== List.sort expected
    it "deletes cascades with self reference to the whole chain" $ runDb $ do
        k1 <- insert $ Chain2 1 Nothing
        k2 <- insert $ Chain2 2 (Just k1)
        insert_ $ Chain2 3 (Just k2)
        delete k1
        cs <- selectList [] []
        let expected = [] :: [Entity Chain2]
        cs @== expected
    it "deletes cascades with field self reference to the whole chain" $ runDb $ do
        k1 <- insert $ Chain3 1 Nothing
        k2 <- insert $ Chain3 2 (Just k1)
        insert_ $ Chain3 3 (Just k2)
        delete k1
        cs <- selectList [] []
        let expected = [] :: [Entity Chain3]
        cs @== expected

    describe "EntityDef" $ do
        let ed =
                entityDef (Proxy @SimpleCascadeChild)
            isRefCol =
                (FieldNameHS "ref" ==) . fieldHaskell
            expected = FieldCascade
                { fcOnUpdate = Nothing
                , fcOnDelete = Just Cascade
                }
            Just refField =
                List.find isRefCol (getEntityFields ed)

        it "parses into fieldCascade"  $ do
            fieldCascade refField `shouldBe` expected

        it "shouldn't have cascade in extras" $ do
            entityExtra ed
                `shouldBe`
                    mempty

cleanDB :: (MonadIO m) => SqlPersistT m ()
cleanDB = do
    del @SimpleCascadeChild
    del @SimpleCascade
    del @Parent
    del @ParentComposite
    del @ParentImplicit
    del @Child
    del @ChildComposite
    del @ChildImplicit
    del @SelfReferenced
    del @A
    del @AComposite
    del @B
    del @BExplicit
    del @BComposite
    del @Chain
    del @Chain2

del
    :: forall a m.
    ( PersistEntity a
    , PersistEntityBackend a ~ SqlBackend
    , MonadIO m
    )
    => SqlPersistT m ()
del = deleteWhere @_ @_ @a []
