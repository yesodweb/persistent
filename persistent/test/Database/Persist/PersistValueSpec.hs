module Database.Persist.PersistValueSpec where

import Test.Hspec
import Database.Persist.PersistValue
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8


spec :: Spec
spec = describe "PersistValueSpec" $ do
    describe "PersistValue" $ do
        describe "Aeson" $ do
            let
                testPrefix constr prefixChar bytes =
                    takePrefix (toJSON (constr (BS8.pack bytes)))
                    ===
                    String (T.singleton prefixChar)
                roundTrip constr bytes =
                    fromJSON (toJSON (constr (BS8.pack bytes)))
                    ===
                    Data.Aeson.Success (constr (BS8.pack bytes))
                subject constr prefixChar = do
                    prop ("encodes with a " ++ [prefixChar] ++ " prefix") $
                        testPrefix constr prefixChar
                    prop "Round Trips" $
                        roundTrip constr

            describe "PersistDbSpecific" $ do
                subject (PersistLiteral_ DbSpecific) 'p'
            describe "PersistLiteral" $ do
                subject PersistLiteral 'l'
            describe "PersistLiteralEscaped" $ do
                subject PersistLiteralEscaped 'e'

takePrefix :: Value -> Value
takePrefix (String a) = String (T.take 1 a)
takePrefix a = a
