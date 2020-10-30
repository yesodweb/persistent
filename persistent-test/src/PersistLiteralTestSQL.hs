{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module PersistLiteralTestSQL (specsWith) where

import Init
import qualified Data.Text as T
import NullableGenerated

share [mkPersist sqlSettings { mpsGeneric = True }] [persistLowerCase|
  PersistLiteralFieldTestTable
    fieldOne Text
    fieldTwo Text
    fieldThree (NullableGenerated Text)
    deriving Show Eq
|]

specsWith :: Runner SqlBackend m => RunDb SqlBackend m -> Spec
specsWith runDB = describe "PersistLiteral field" $ do
  it "should read a GENERATED column" $ runDB $ do
    -- we have to manage the table manually because of the generated field
    _ <- rawExecute "DROP TABLE IF EXISTS persist_literal_field_test_table;" []
    _ <- rawExecute "CREATE TABLE persist_literal_field_test_table (id serial primary key, field_one text, field_two text, field_three text GENERATED ALWAYS AS (COALESCE(field_one, field_two)) STORED);" []

    insert_ $ PersistLiteralFieldTestTable "ValueForFieldOne" "ValueForFieldTwo" (NullableGenerated Nothing)
    Just (Entity _ PersistLiteralFieldTestTable{..}) <- selectFirst [] []

    liftIO $ (Just "ValueForFieldOne") @?= (unNullableGenerated persistLiteralFieldTestTableFieldThree)
