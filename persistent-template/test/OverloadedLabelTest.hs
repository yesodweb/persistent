{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module OverloadedLabelTest where

import           TemplateTestImports

mkPersist sqlSettings [persistUpperCase|

User
    name    String
    age     Int

Dog
    userId  UserId
    name    String
    age     Int

Organization
    name    String

|]

spec :: Spec
spec = describe "OverloadedLabels" $ do
    it "works for monomorphic labels" $ do
        let UserName = #name
            OrganizationName = #name
            DogName = #name

        compiles

    it "works for polymorphic labels" $ do
        let name :: _ => EntityField rec a
            name = #name

            UserName = name
            OrganizationName = name
            DogName = name

        compiles

compiles :: Expectation
compiles = True `shouldBe` True
