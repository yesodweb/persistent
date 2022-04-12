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

module Database.Persist.TH.TypeLitFieldDefsSpec where

import GHC.TypeLits
import TemplateTestImports

newtype Finite (n :: Nat) = Finite Int

instance PersistField (Finite n) where
    toPersistValue (Finite n) = toPersistValue n
    fromPersistValue = fmap Finite . fromPersistValue

instance PersistFieldSql (Finite n) where
    sqlType _ = sqlType (Proxy :: Proxy Int)

newtype Labelled (t :: Symbol) = Labelled Int

instance PersistField (Labelled n) where
    toPersistValue (Labelled n) = toPersistValue n
    fromPersistValue = fmap Labelled . fromPersistValue

instance PersistFieldSql (Labelled n) where
    sqlType _ = sqlType (Proxy :: Proxy Int)

mkPersist sqlSettings [persistLowerCase|
WithFinite
    one    (Finite 1)
    twenty (Finite 20)

WithLabelled
    one    (Labelled "one")
    twenty (Labelled "twenty")
|]

spec :: Spec
spec = describe "TypeLitFieldDefs" $ do
    it "should support numeric type literal fields in entity definition" $ do
        let mkFinite :: Finite 1 -> Finite 20 -> WithFinite
            mkFinite = WithFinite
        compiles

    it "should support string based type literal fields in entity definition" $ do
        let mkLabelled :: Labelled "one" -> Labelled "twenty" -> WithLabelled
            mkLabelled = WithLabelled
        compiles

compiles :: Expectation
compiles = True `shouldBe` True
