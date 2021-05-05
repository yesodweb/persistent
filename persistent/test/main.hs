module Main where

import Test.Hspec

import qualified Database.Persist.ClassSpec as ClassSpec
import qualified Database.Persist.PersistValueSpec as PersistValueSpec
import qualified Database.Persist.QuasiSpec as QuasiSpec
import qualified Database.Persist.THSpec as THSpec

main :: IO ()
main = hspec $ do
    describe "Database" $ describe "Persist" $ do
        THSpec.spec
        QuasiSpec.spec
        ClassSpec.spec
        PersistValueSpec.spec
