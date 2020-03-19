{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module ForeignKey where

import Init

-- mpsGeneric = False is due to a bug or at least lack of a feature in mkKeyTypeDec TH.hs
share [mkPersist persistSettings { mpsGeneric = False }, mkMigrate "compositeMigrate"] [persistLowerCase|
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
specsWith runDb = describe "foreign keys options" $ do
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
