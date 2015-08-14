{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CustomPersistField where

import Init
import Data.Text (pack)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (toStrict, fromStrict)
import Data.String (IsString)

newtype Markdown = Markdown TL.Text
  deriving (Eq, Ord, Monoid, IsString, Show)

instance PersistField Markdown where
  toPersistValue (Markdown t) = PersistText $ toStrict t
  fromPersistValue (PersistText t) = Right $ Markdown $ fromStrict t
  fromPersistValue wrongValue = Left $ pack $ "Yesod.Text.Markdown: When attempting to create Markdown from a PersistValue, received " ++ show wrongValue ++ " when a value of type PersistText was expected."

instance PersistFieldSql Markdown where
    sqlType _ = SqlString