{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Postgresql.Util (
  mkPostgresUpdateText
) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Database.Persist (FieldNameDB, PersistEntity (..), PersistUpdate(..), Update(..), updateUpdate, fieldDB, FieldDef)


mkPostgresUpdateText :: PersistEntity record => (FieldNameDB -> Text) -> (Text -> Text) -> Update record -> Text
mkPostgresUpdateText escapeName refColumn x =
  case updateUpdate x of
    Assign -> n <> "=?"
    Add -> T.concat [n, "=EXCLUDED.", refColumn n, "+?"]
    Subtract -> T.concat [n, "=EXCLUDED.", refColumn n, "-?"]
    Multiply -> T.concat [n, "=EXCLUDED.", refColumn n, "*?"]
    Divide -> T.concat [n, "=EXCLUDED.", refColumn n, "/?"]
    BackendSpecificUpdate up ->
      error . T.unpack $ "mkUpdateText: BackendSpecificUpdate " <> up <> " not supported"
  where
    n = escapeName . fieldDB . updateFieldDef $ x

-- | Gets the 'FieldDef' for an 'Update'.
updateFieldDef :: PersistEntity v => Update v -> FieldDef
updateFieldDef (Update f _ _) = persistFieldDef f
updateFieldDef BackendUpdate {} = error "updateFieldDef: did not expect BackendUpdate"