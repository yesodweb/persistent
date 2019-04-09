{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
module TreeTest where

import Test.Hspec.Expectations ()
import Init
import qualified Data.Map as Map

import Data.Maybe (isJust)
import Database.Persist.TH (mkDeleteCascade)


-- mpsGeneric = False is due to a bug or at least lack of a feature in
-- mkKeyTypeDec TH.hs
share
    [ mkPersist persistSettings { mpsGeneric = False }
    , mkMigrate "compositeMigrate"
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
