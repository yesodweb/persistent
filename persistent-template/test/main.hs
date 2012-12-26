{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.ByteString.Lazy.Char8 ()
import Test.QuickCheck.Arbitrary
import Control.Applicative ((<$>), (<*>))

import Database.Persist
import Database.Persist.Store (DeleteCascade (..))
import Database.Persist.TH
import Data.Text (Text, pack)
import Data.Aeson

share [mkPersist sqlSettings { mpsGeneric = False }, mkDeleteCascade sqlSettings { mpsGeneric = False }] [persistUpperCase|
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
instance ToJSON NoJson where
    toJSON = undefined
instance FromJSON NoJson where
    parseJSON = undefined

arbitraryT = pack <$> arbitrary

instance Arbitrary Person where
    arbitrary = Person <$> arbitraryT <*> arbitrary <*> arbitrary
instance Arbitrary Address where
    arbitrary = Address <$> arbitraryT <*> arbitraryT <*> arbitrary

main :: IO ()
main = hspec $ do
    describe "JSON serialization" $ do
        prop "to/from is idempotent" $ \person ->
            decode (encode person) == Just (person :: Person)
        it "decode" $
            decode "{\"Name\":\"Michael\",\"age\":27,\"address\":{\"street\":\"Narkis\",\"city\":\"Maalot\"}}" `shouldBe` Just
                (Person "Michael" (Just 27) $ Address "Narkis" "Maalot" Nothing)
