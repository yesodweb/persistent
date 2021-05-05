{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.TH.MaybeFieldDefsSpec where

import TemplateTestImports

mkPersist sqlSettings [persistLowerCase|
Account
    name    (Maybe String)
    email   String
|]

spec :: Spec
spec = describe "MaybeFieldDefs" $ do
    it "should support literal `Maybe` declaration in entity definition" $ do
        let mkAccount :: Maybe String -> String -> Account
            mkAccount = Account
        compiles

compiles :: Expectation
compiles = True `shouldBe` True
