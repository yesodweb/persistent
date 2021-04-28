{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Char as Char
import Data.List
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
#if !MIN_VERSION_base(4,11,0)
-- This can be removed when GHC < 8.2.2 isn't supported anymore
import Data.Semigroup ((<>))
#endif
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.Time
import Text.Shakespeare.Text

import Database.Persist.Class.PersistField
import Database.Persist.Quasi
import Database.Persist.Quasi.Internal
       ( Line(..)
       , LinesWithComments(..)
       , Token(..)
       , UnboundEntityDef(..)
       , UnboundForeignDef(..)
       , associateLines
       , parseFieldType
       , parseLine
       , preparse
       , splitExtras
       , takeColsEx
       )
import Database.Persist.Types
import Database.Persist.EntityDef.Internal

import qualified Database.Persist.THSpec as THSpec
import qualified Database.Persist.QuasiSpec as QuasiSpec

main :: IO ()
main = hspec $ do
    describe "Database" $ describe "Persist" $ do
        THSpec.spec
        QuasiSpec.spec

    describe "fromPersistValue" $
        describe "UTCTime" $
            it "works with format" $
                fromPersistValue (PersistText "2018-02-27 10:49:42.123")
                    `shouldBe` Right (UTCTime (fromGregorian 2018 02 27) (timeOfDayToTime (TimeOfDay 10 49 42.123)))

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

arbitraryWhiteSpaceChar :: Gen Char
arbitraryWhiteSpaceChar =
  oneof $ pure <$> [' ', '\t', '\n', '\r']
