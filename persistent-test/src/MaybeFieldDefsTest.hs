module MaybeFieldDefsTest (specsWith) where

import Data.String (IsString)

import Init

share [mkPersist sqlSettings,  mkMigrate "maybeFieldDefMigrate"] [persistLowerCase|
  MaybeFieldDefEntity
    optionalString    (Maybe String)
    optionalInt       (Maybe Int)
|]

specsWith :: Runner backend m => RunDb backend m -> Spec
specsWith runDb = describe "Maybe Field Definitions" $ do
  it "runs appropriate migrations" $ runDb $ do
    runMigration maybeFieldDefMigrate

    emptyEntity <- insert $ MaybeFieldDefEntity Nothing Nothing
    get emptyEntity `shouldReturn` MaybeFieldDefEntity Nothing Nothing
    populatedEntity <- insert $ MaybeFieldDefEntity (Just "text") (Just 8)
    get populatedEntity `shouldReturn` MaybeFieldDefEntity (Just "text") (Just 8)
