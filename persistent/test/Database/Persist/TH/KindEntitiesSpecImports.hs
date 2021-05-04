{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Database.Persist.TH.KindEntitiesSpecImports where

import Data.Proxy
import qualified Data.Text as T
import TemplateTestImports

data Owner = MerchantOwned | CustomerOwned
data AccountKind = Debit | Credit

newtype MoneyAmount (a :: Owner) (b :: AccountKind) = MoneyAmount Rational

instance PersistFieldSql (MoneyAmount a b) where
    sqlType _ = sqlType (Proxy :: Proxy Rational)

instance PersistField (MoneyAmount a b) where
    toPersistValue (MoneyAmount n) =
        toPersistValue n
    fromPersistValue v =
        MoneyAmount <$> fromPersistValue v
