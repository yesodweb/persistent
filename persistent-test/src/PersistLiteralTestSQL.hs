{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module PersistLiteralTestSQL (specsWith) where

import Init
import qualified Data.Text as T

share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateAll"] [persistLowerCase|
PersistLiteralFieldTestTable
  fieldOne Text Maybe
  fieldTwo Text Maybe
  fieldThree Text Maybe generated=COALESCE(field_one,field_two)
  deriving Show Eq
|]

specsWith :: Runner SqlBackend m => RunDb SqlBackend m -> Spec
specsWith runDB = describe "PersistLiteral field" $ do
  it "should read a generated column" $ runDB $ do
    runMigration migrateAll
    insert_ $ PersistLiteralFieldTestTable
      { persistLiteralFieldTestTableFieldOne = Just "like, literally this exact string"
      , persistLiteralFieldTestTableFieldTwo = Just "like, literally some other string"
      , persistLiteralFieldTestTableFieldThree = Nothing
      }
    Just (Entity _ PersistLiteralFieldTestTable{..}) <- selectFirst [] []
    liftIO $ persistLiteralFieldTestTableFieldThree @?= Just "like, literally this exact string"
