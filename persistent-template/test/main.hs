{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit()
import Test.HUnit
import Data.ByteString.Lazy.Char8 ()
import Test.QuickCheck.Arbitrary
import Control.Applicative ((<$>), (<*>))

import Database.Persist
import Database.Persist.TH
import Data.Text (Text, pack)
import Data.Aeson

mkPersist sqlSettings [persistUpperCase|
Person json
    name Text
    age Int Maybe
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
|]

-- ensure no-json works
instance ToJSON (NoJsonGeneric b) where
    toJSON = undefined
instance FromJSON (NoJsonGeneric b) where
    parseJSON = undefined

arbitraryT = pack <$> arbitrary

instance Arbitrary (PersonGeneric b) where
    arbitrary = Person <$> arbitraryT <*> arbitrary <*> arbitrary
instance Arbitrary (AddressGeneric b) where
    arbitrary = Address <$> arbitraryT <*> arbitraryT <*> arbitrary

main :: IO ()
main = hspec $ do
    describe "JSON serialization" $ do
        prop "to/from is idempotent" $ \person ->
            decode (encode person) == Just (person :: Person)
        it "decode" $
            decode "{\"name\":\"Michael\",\"age\":27,\"address\":{\"street\":\"Narkis\",\"city\":\"Maalot\"}}" @?= Just
                (Person "Michael" (Just 27) $ Address "Narkis" "Maalot" Nothing)
