{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module TemplateTestImports
    ( module TemplateTestImports
    , module X
    ) where

import Data.Aeson.TH
import Test.QuickCheck

import Data.Int as X
import Database.Persist.Sql as X
import Database.Persist.TH as X
import Test.Hspec as X
import Data.Proxy as X
import Data.Text as X (Text)

data Foo = Bar | Baz
    deriving (Show, Eq)

deriveJSON defaultOptions ''Foo

derivePersistFieldJSON "Foo"

instance Arbitrary Foo where
    arbitrary = elements [Bar, Baz]
