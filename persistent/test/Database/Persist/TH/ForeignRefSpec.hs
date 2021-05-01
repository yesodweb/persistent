{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
--
-- DeriveAnyClass is not actually used by persistent-template
-- But a long standing bug was that if it was enabled, it was used to derive instead of GeneralizedNewtypeDeriving
-- This was fixed by using DerivingStrategies to specify newtype deriving should be used.
-- This pragma is left here as a "test" that deriving works when DeriveAnyClass is enabled.
-- See https://github.com/yesodweb/persistent/issues/578
{-# LANGUAGE DeriveAnyClass #-}

module Database.Persist.TH.ForeignRefSpec where

import Control.Applicative (Const(..))
import Data.Aeson
import Data.ByteString.Lazy.Char8 ()
import Data.Coerce
import Data.Functor.Identity (Identity(..))
import Data.Int
import qualified Data.List as List
import Data.Proxy
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (Gen)

import Database.Persist
import Database.Persist.EntityDef.Internal
import Database.Persist.Sql
import Database.Persist.Sql.Util
import Database.Persist.TH
import TemplateTestImports

mkPersist sqlSettings [persistLowerCase|

ForeignTarget
    name Text
    deriving Eq Show

ForeignSource
    name Text
    foreignTargetId ForeignTargetId
    Foreign ForeignTarget fk_s_t foreignTargetId

ForeignPrimary
    name Text
    Primary name
    deriving Eq Show

ForeignPrimarySource
    name Text
    Foreign ForeignPrimary fk_name_target name

NullableRef
    name Text Maybe
    Foreign ForeignPrimary fk_nullable_ref name
|]

spec :: Spec
spec = describe "ForeignRefSpec" $ do
    it "should compile" $ do
        True `shouldBe` True

    describe "ForeignPrimarySource" $ do
        let
            fpsDef =
                entityDef $ Proxy @ForeignPrimarySource
            [foreignDef] =
                entityForeigns fpsDef
        it "has the right type" $ do
            foreignPrimarySourceFk_name_target (ForeignPrimarySource "asdf")
                `shouldBe`
                    ForeignPrimaryKey "asdf"
