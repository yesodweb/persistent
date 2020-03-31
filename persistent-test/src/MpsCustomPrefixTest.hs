module MpsCustomPrefixTest where

import Init
import PersistentTestModels

specsWith :: MonadIO m => RunDb SqlBackend m -> Spec
specsWith runDb = describe "mpsCustomPrefix" $
  it "works" $ runDb $ do
    deleteWhere ([] :: [Filter CustomPrefix2])
    deleteWhere ([] :: [Filter CustomPrefix1])
    cp1a <- insert $ CustomPrefix1 1
    update cp1a [CP1CustomFieldName =. 2]
    cp1b <- insert $ CustomPrefix1 3
    cp2 <- insert $ CustomPrefix2 4 cp1a
    update cp2 [CP2CustomPrefixedRef =. cp1b, CP2OtherCustomFieldName =. 5]

    mcp1a <- get cp1a
    liftIO $ mcp1a @?= Just (CustomPrefix1 2)
    liftIO $ fmap _cp1CustomFieldName mcp1a @?= Just 2
    mcp2 <- get cp2
    liftIO $ fmap _cp2CustomPrefixedRef mcp2 @?= Just cp1b
    liftIO $ fmap _cp2OtherCustomFieldName mcp2 @?= Just 5

    cpls <- insert $ CPCustomPrefixedLeftSum 5
    cprs <- insert $ CPCustomPrefixedRightSum "Hello"
    update cpls [CPCustomPrefixedLeft =. 6]
    update cprs [CPCustomPrefixedRight =. "World"]
    mcpls <- get cpls
    mcprs <- get cprs

    liftIO $ mcpls @?= Just (CPCustomPrefixedLeftSum 6)
    liftIO $ mcprs @?= Just (CPCustomPrefixedRightSum "World")
