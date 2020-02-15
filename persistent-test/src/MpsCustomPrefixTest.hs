module MpsCustomPrefixTest where

import Init
import PersistentTestModels

specsWith :: MonadIO m => RunDb SqlBackend m -> Spec
specsWith runDb = describe "mpsCustomPrefix" $
  it "works" $ runDb $ do
    deleteWhere ([] :: [Filter CustomPrefix2])
    deleteWhere ([] :: [Filter CustomPrefix1])
    cp1a <- insert $ CustomPrefix1 1
    update cp1a [CustomPrefix1CustomFieldName =. 2]
    cp1b <- insert $ CustomPrefix1 3
    cp2 <- insert $ CustomPrefix2 4 cp1a
    update cp2 [CustomPrefix2CustomPrefixedRef =. cp1b, CustomPrefix2OtherCustomFieldName =. 5]

    mcp1a <- get cp1a
    liftIO $ mcp1a @?= Just (CustomPrefix1 2)
    liftIO $ fmap _cp1CustomFieldName mcp1a @?= Just 2
    mcp2 <- get cp2
    liftIO $ fmap _cp2CustomPrefixedRef mcp2 @?= Just cp1b
    liftIO $ fmap _cp2OtherCustomFieldName mcp2 @?= Just 5

    insert_ $ CustomPrefixSumCustomPrefixedLeftSum 5
    insert_ $ CustomPrefixSumCustomPrefixedRightSum "Hello"

    --TODO: test case below works when mpsPrefixFields = False
    -- deleteWhere ([] :: [Filter CustomPrefix2])
    -- deleteWhere ([] :: [Filter CustomPrefix1])
    -- cp1a <- insert $ CustomPrefix1 1
    -- update cp1a [CustomFieldName =. 2]
    -- cp1b <- insert $ CustomPrefix1 3
    -- cp2 <- insert $ CustomPrefix2 4 cp1a
    -- update cp2 [CustomPrefixedRef =. cp1b, OtherCustomFieldName =. 5]

    -- mcp1a <- get cp1a
    -- liftIO $ mcp1a @?= Just (CustomPrefix1 2)
    -- liftIO $ fmap _customFieldName mcp1a @?= Just 2
    -- mcp2 <- get cp2
    -- liftIO $ fmap _customPrefixedRef mcp2 @?= Just cp1b
    -- liftIO $ fmap _otherCustomFieldName mcp2 @?= Just 5

    -- insert_ $ CustomPrefixedLeftSum 5
    -- insert_ $ CustomPrefixedRightSum "Hello"
