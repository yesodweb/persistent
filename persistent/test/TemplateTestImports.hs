{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module TemplateTestImports
    ( module TemplateTestImports
    , module X
    ) where

import Data.Aeson.TH
import Test.QuickCheck

import Control.Monad
import Data.Int as X
import Data.Maybe
import Data.Proxy as X
import Data.Text as X (Text)
import Database.Persist.Sql as X
import Database.Persist.TH as X
import Language.Haskell.TH.Syntax
import Test.Hspec as X

data Foo = Bar | Baz
    deriving (Show, Eq)

deriveJSON defaultOptions ''Foo

derivePersistFieldJSON "Foo"

instance Arbitrary Foo where
    arbitrary = elements [Bar, Baz]
