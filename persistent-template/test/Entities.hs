{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Entities where

import Control.Applicative (Const (..))
import Data.Aeson
import Data.ByteString.Lazy.Char8 ()
import Data.Functor.Identity (Identity (..))
import Data.Text (Text, pack)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (Gen)

import Database.Persist
import Database.Persist.TH
import TemplateTestImports


entityDefs :: [EntityDef]
entityDefs = [persistUpperCase|
Person json
    name Text
    age Int Maybe
    foo Foo
    address Address
    deriving Show Eq
Address json
    street Text
    city Text
    zip Int Maybe
    deriving Show Eq
NoJson
    foo Text
    deriving Show Eq

Thing
    Id Text
    age Int
    deriving Show Eq

Gear
    thing   ThingId
    deriving Show Eq
|]
