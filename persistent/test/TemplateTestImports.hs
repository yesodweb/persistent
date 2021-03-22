{-# LANGUAGE TemplateHaskell #-}

module TemplateTestImports
    ( module TemplateTestImports
    , module X
    ) where

import Data.Aeson.TH
import Test.QuickCheck

import Test.Hspec as X
import Database.Persist.Sql as X
import Database.Persist.TH as X

data Foo = Bar | Baz
    deriving (Show, Eq)

deriveJSON defaultOptions ''Foo

derivePersistFieldJSON "Foo"

instance Arbitrary Foo where
    arbitrary = elements [Bar, Baz]
