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
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Database.Persist.TH.NestedSymbolsInTypeSpec where

import Data.Map
import Database.Persist.TH.NestedSymbolsInTypeSpecImports
import TemplateTestImports

mkPersist
    sqlSettings
    [persistLowerCase|
PathEntitySimple
    readOnly  (Maybe (SomePath ReadOnly))

PathEntityNested
    paths  (Maybe (Map Text [SomePath ReadWrite]))
|]

spec :: Spec
spec = describe "NestedSymbolsInType" $ do
    it "should support nested parens" $ do
        let
            mkPathEntitySimple :: Maybe (SomePath ReadOnly) -> PathEntitySimple
            mkPathEntitySimple = PathEntitySimple
            pathEntitySimpleReadOnly' :: PathEntitySimple -> Maybe (SomePath ReadOnly)
            pathEntitySimpleReadOnly' = pathEntitySimpleReadOnly
        compiles

    it "should support deeply nested parens + square brackets" $ do
        let
            mkPathEntityNested :: Maybe (Map Text [SomePath ReadWrite]) -> PathEntityNested
            mkPathEntityNested = PathEntityNested
            pathEntityNestedPaths'
                :: PathEntityNested -> Maybe (Map Text [SomePath ReadWrite])
            pathEntityNestedPaths' = pathEntityNestedPaths
        compiles

compiles :: Expectation
compiles = True `shouldBe` True
