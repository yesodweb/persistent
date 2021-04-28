{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Database.Persist.TH.KindEntitiesSpecImports where

import qualified Data.Text as T
import TemplateTestImports

data Owner = MerchantOwned | CustomerOwned
data AccountKind = Debit | Credit

newtype MoneyAmount (a :: Owner) (b :: AccountKind) = MoneyAmount Rational

instance PersistFieldSql (MoneyAmount a b) where
    sqlType _ = SqlInt64

instance PersistField (MoneyAmount a b) where
    toPersistValue (MoneyAmount n) = PersistRational n
    fromPersistValue = \case
      PersistRational n -> pure (MoneyAmount n)
      x -> Left $ T.pack $ "Failed to read MoneyAmount: " ++ show x
