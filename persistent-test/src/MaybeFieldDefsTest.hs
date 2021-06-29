{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module MaybeFieldDefsTest (specsWith, maybeFieldDefMigrate) where

import Data.String (IsString)

import Init

share [mkPersist sqlSettings { mpsGeneric = True },  mkMigrate "maybeFieldDefMigrate"] [persistLowerCase|
  MaybeFieldDefEntity
    optionalString    (Maybe String)
    optionalInt       (Maybe Int)
    deriving Eq Show
|]

specsWith :: (Runner backend m) => RunDb backend m -> Spec
specsWith runDb = describe "Maybe Field Definitions" $ do
  it "runs appropriate migrations" $ runDb $ do
    emptyEntity <- insert $ MaybeFieldDefEntity Nothing Nothing
    emptyResult <- get emptyEntity
    liftIO $ emptyResult @?= Just (MaybeFieldDefEntity Nothing Nothing)
    populatedEntity <- insert $ MaybeFieldDefEntity (Just "text") (Just 8)
    populatedResult <- get populatedEntity
    liftIO $ populatedResult @?= Just (MaybeFieldDefEntity (Just "text") (Just 8))
