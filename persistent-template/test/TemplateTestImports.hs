{-# LANGUAGE TemplateHaskell #-}
module TemplateTestImports where

import Data.Aeson.TH
import Test.QuickCheck

import Database.Persist.TH

data Foo = Bar | Baz
    deriving (Show, Eq)

deriveJSON defaultOptions ''Foo

derivePersistFieldJSON "Foo"

instance Arbitrary Foo where
    arbitrary = elements [Bar, Baz]
