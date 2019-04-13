{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
module TreeTest where

import Database.Persist.TH (mkDeleteCascade)

import Init


-- mpsGeneric = False is due to a bug or at least lack of a feature in
-- mkKeyTypeDec TH.hs
share
    [ mkPersist persistSettings { mpsGeneric = False }
    , mkMigrate "treeMigrate"
    , mkDeleteCascade persistSettings { mpsGeneric = False } ] [persistLowerCase|
  Tree
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
specsWith runDb = describe "tree" $
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
