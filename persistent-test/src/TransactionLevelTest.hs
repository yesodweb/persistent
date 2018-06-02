{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans -O0 #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module TransactionLevelTest where

import           Init
#ifndef WITH_NOSQL

share [mkPersist sqlSettings, mkMigrate "migration"] [persistUpperCase|
  Wombat
     name        Text sqltype=varchar(80)

     Primary name
     deriving Eq Show Ord

|]

specs :: Spec
specs = describe "IsolationLevel" $ do
  let item = Wombat "uno"
      isolationLevels = [minBound..maxBound]
  forM_ isolationLevels $ \il -> describe "insertOnDuplicateKeyUpdate" $ do
    it (show il ++ " works") $ db $ do
      transactionUndoWithIsolation il
      deleteWhere ([] :: [Filter Wombat])
      _ <- insert item
      Just item' <- get (WombatKey "uno")
      item' @== item
#else
specs :: Spec
specs = describe "IsolationLevel" $ do
  it "Is only supported on SQL variants." $ do
    True `shouldBe` True
#endif
