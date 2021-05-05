{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards, UndecidableInstances #-}

module TreeTest where

import Init


-- mpsGeneric = False is due to a bug or at least lack of a feature in
-- mkKeyTypeDec TH.hs
share
    [ mkPersist persistSettings { mpsGeneric = False }
    , mkMigrate "treeMigrate"
    ] [persistLowerCase|
  Tree sql=trees
      name    Text
      parent  Text Maybe
      Primary name
      Foreign Tree fkparent parent
|]


cleanDB
    :: (PersistQuery backend, PersistEntityBackend Tree ~ backend, MonadIO m)
    => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Tree])

specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "tree" $ do
    it "Tree relationships" $ runDb $ do
        kgp@(TreeKey gpt) <- insert $ Tree "grandpa" Nothing
        kdad@(TreeKey dadt) <- insert $ Tree "dad" $ Just gpt
        kc <- insert $ Tree "child" $ Just dadt
        c <- getJust kc
        treeFkparent c @== Just kdad
        dad <- getJust kdad
        treeFkparent dad @== Just kgp
        gp <- getJust kgp
        treeFkparent gp @== Nothing
    describe "entityDef" $ do
        let ed = entityDef (Proxy :: Proxy Tree)
        it "has the right haskell name" $ do
            getEntityHaskellName ed `shouldBe` EntityNameHS "Tree"
        it "has the right DB name" $ do
            getEntityDBName ed `shouldBe` EntityNameDB "trees"

    describe "foreign ref" $ do
        let [ForeignDef{..}] = getEntityForeignDefs (entityDef (Proxy :: Proxy Tree))
        it "has the right haskell name" $ do
            foreignRefTableHaskell `shouldBe`
                EntityNameHS "Tree"
        it "has the right db name" $ do
            foreignRefTableDBName `shouldBe`
                EntityNameDB "trees"
        it "has the right constraint name" $ do
            foreignConstraintNameHaskell `shouldBe`
                ConstraintNameHS "fkparent"
        it "has the right DB constraint name" $ do
            foreignConstraintNameDBName `shouldBe`
                ConstraintNameDB "treefkparent"
        it "has the right fields" $ do
            foreignFields `shouldBe`
                [ ( (FieldNameHS "parent", FieldNameDB "parent")
                  , (FieldNameHS "name", FieldNameDB "name")
                  )
                ]
