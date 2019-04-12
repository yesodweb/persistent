-- This module is used for CustomPersistFieldTest; the TH GHC stage restriction requires it to be here.
-- The code is taken from the Yesod.Text.Markdown package; see https://github.com/yesodweb/persistent/issues/448
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CustomPersistField where

import Data.String (IsString)
import Data.Text (pack)
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text.Lazy as TL (Text)

import Init

newtype Markdown = Markdown TL.Text
  deriving (Eq, Ord, IsString, Show)

instance PersistField Markdown where
  toPersistValue (Markdown t) = PersistText $ toStrict t
  fromPersistValue (PersistText t) = Right $ Markdown $ fromStrict t
  fromPersistValue wrongValue = Left $ pack $ "Received " ++ show wrongValue ++ " when a value of type PersistText was expected."


instance PersistFieldSql Markdown where
    sqlType _ = SqlString
