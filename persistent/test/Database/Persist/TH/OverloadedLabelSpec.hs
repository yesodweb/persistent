{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wname-shadowing -Werror=name-shadowing #-}

module Database.Persist.TH.OverloadedLabelSpec where

import TemplateTestImports

mkPersist
    sqlSettings
    [persistUpperCase|

User
    name    String
    age     Int

Dog
    userId  UserId
    name    String
    age     Int

Organization
    name    String

Student
    userId  UserId
    departmentName String
    Primary userId
|]

spec :: Spec
spec = describe "OverloadedLabels" $ do
    it "works for monomorphic labels" $ do
        let
            UserName = #name
            OrganizationName = #name
            DogName = #name

        compiles

    it "works for polymorphic labels" $ do
        let
            name :: (_) => EntityField rec a
            name = #name

            UserName = name
            OrganizationName = name
            DogName = name

        compiles

    it "works for id labels" $ do
        let
            UserId = #id
            orgId = #id :: EntityField Organization OrganizationId

        compiles

    it "works for Primary labels" $ do
        let
            StudentId = #id
            studentId = #id :: EntityField Student StudentId

        compiles

compiles :: IO ()
compiles = pure ()
