{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.TH.RequireOnlyPersistImportSpec where

-- This test asserts this is the only import required to define entities
-- See: https://github.com/yesodweb/persistent/pull/1369
import Database.Persist.TH

-- always explicitly import qualified Hspec in the context of this spec
import qualified Test.Hspec as HS

mkPersist
    sqlSettings
    [persistLowerCase|
Plain
    name String
    age  Int
    deriving Show Eq

JsonEncoded json
    name String
    age  Int
    deriving Show Eq
|]

spec :: HS.Spec
spec =
    HS.describe "RequireOnlyPersistImport" $ do
        HS.it "Plain" $ do
            let
                typeSigPlain :: String -> Int -> Plain
                typeSigPlain = Plain
            compiles

        HS.it "JsonEncoded" $ do
            let
                typeSigJsonEncoded :: String -> Int -> JsonEncoded
                typeSigJsonEncoded = JsonEncoded
            compiles

compiles :: HS.Expectation
compiles = True `HS.shouldBe` True
