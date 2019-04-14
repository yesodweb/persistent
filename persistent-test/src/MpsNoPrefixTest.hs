module MpsNoPrefixTest where

import Init
import PersistentTestModels

specsWith :: MonadIO m => RunDb SqlBackend m -> Spec
specsWith runDb = describe "mpsNoPrefix" $ do
  it "works" $ runDb $ do
    deleteWhere ([] :: [Filter NoPrefix2])
    deleteWhere ([] :: [Filter NoPrefix1])
    np1a <- insert $ NoPrefix1 1
    update np1a [SomeFieldName =. 2]
    np1b <- insert $ NoPrefix1 3
    np2 <- insert $ NoPrefix2 4 np1a
    update np2 [UnprefixedRef =. np1b, SomeOtherFieldName =. 5]

    mnp1a <- get np1a
    liftIO $ mnp1a @?= Just (NoPrefix1 2)
    liftIO $ fmap someFieldName mnp1a @?= Just 2
    mnp2 <- get np2
    liftIO $ fmap unprefixedRef mnp2 @?= Just np1b
    liftIO $ fmap someOtherFieldName mnp2 @?= Just 5

    insert_ $ UnprefixedLeftSum 5
    insert_ $ UnprefixedRightSum "Hello"

  it "IsSqlKey instance" $ runDb $ do
    let p = Person "Alice" 30 Nothing
    key@(PersonKey (SqlBackendKey i)) <- insert p
    liftIO $ fromSqlKey key `shouldBe` (i :: Int64)
    mp <- get $ toSqlKey i
    liftIO $ mp `shouldBe` Just p
